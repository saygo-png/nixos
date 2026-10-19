{
  lib,
  pkgs,
  config,
  conUsername,
  ...
}: {
  # Reference https://github.com/iynaix/dotfiles/blob/32e43c330cca0b52f584d0007fe64746994233b0/nixos/impermanence.nix
  options.custom.persist = let
    assertNoHomeDirs = paths:
      assert (lib.assertMsg (!lib.any (lib.hasPrefix "/home") paths) "/home used in a root persist!"); paths;
    mkPathsOption = apply: description:
      lib.mkOption {
        inherit apply description;
        type = lib.types.listOf lib.types.str;
        default = [];
      };
    rootPaths = mkPathsOption assertNoHomeDirs;
    homePaths = mkPathsOption lib.id;
  in {
    enable = lib.mkEnableOption "Impermanence" // {default = true;};
    root = {
      directories = rootPaths "Directories to persist in root filesystem";
      files = rootPaths "Files to persist in root filesystem";
      cache = {
        directories = rootPaths "Root directories to persist, but not to snapshot";
        files = rootPaths "Root files to persist, but not to snapshot";
      };
    };
    home = {
      directories = homePaths "Directories to persist in home directory";
      files = homePaths "Files to persist in home directory";
      cache = {
        directories = homePaths "Home directories to persist, but not to snapshot";
        files = homePaths "Home files to persist, but not to snapshot";
      };
    };
  };

  config = lib.mkIf config.custom.persist.enable {
    security.sudo.extraConfig = "Defaults lecture=never";

    fileSystems = {
      "/".neededForBoot = true;
      "/nix".neededForBoot = true;
      "/boot".neededForBoot = true;
      "/cache".neededForBoot = true;
      "/persist".neededForBoot = true;
    };

    boot.initrd.systemd.services.rollback = {
      description = "Rollback root filesystem to a pristine state on boot";
      wantedBy = ["initrd.target"];
      after = ["zfs-import-zroot.service"];
      before = ["sysroot.mount"];
      path = [pkgs.zfs];
      unitConfig.DefaultDependencies = "no";
      serviceConfig.Type = "oneshot";
      script = ''
        zfs rollback -r zroot/local/root@blank && echo ">> rollback complete <<" || echo "!! rollback failed !!"
      '';
    };

    environment.persistence = let
      cfg = config.custom.persist;
    in {
      "/persist" = {
        hideMounts = false;
        files =
          lib.unique cfg.root.files;
        directories = lib.unique (
          [
            "/var/log" # systemd journal is stored in /var/log/journal
            "/var/lib/nixos" # user uids and gids
            "/var/lib/systemd/coredump"
          ]
          ++ cfg.root.directories
        );

        users.${conUsername} = {
          files = lib.unique cfg.home.files;
          directories = lib.unique cfg.home.directories;
        };
      };

      # Cache are files that should be persisted, but not backed up
      "/cache" = {
        hideMounts = true;
        files = lib.unique (["/etc/machine-id"] ++ cfg.root.cache.files);
        directories = lib.unique cfg.root.cache.directories;

        users.${conUsername} = {
          files = lib.unique cfg.home.cache.files;
          directories = lib.unique cfg.home.cache.directories;
        };
      };
    };
  };
}
