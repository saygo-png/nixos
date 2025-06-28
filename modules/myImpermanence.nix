{
  conUsername,
  pkgs,
  lib,
  ...
}: {
  security.sudo.extraConfig = "Defaults lecture=never";

  fileSystems = {
    "/".neededForBoot = true;
    "/nix".neededForBoot = true;
    "/boot".neededForBoot = true;
    "/cache".neededForBoot = true;
    "/persist".neededForBoot = true;
  };

  boot.initrd.postResumeCommands = lib.mkAfter ''
    zfs rollback -r zroot/local/root@blank
  '';

  # FIX: replace above with this
  # boot.initrd.systemd.services.rollback = {
  #   description = "Rollback root filesystem to a pristine state on boot";
  #   wantedBy = ["initrd.target"];
  #   after = ["zfs-import-zroot.service"];
  #   before = ["sysroot.mount"];
  #   path = [pkgs.zfs];
  #   unitConfig.DefaultDependencies = "no";
  #   serviceConfig.Type = "oneshot";
  #   script = ''
  #     zfs rollback -r zroot/local/root@blank && echo ">> rollback complete <<" || echo "!! rollback failed !!"
  #   '';
  # };

  environment.persistence = {
    "/persist" = {
      directories = [
        "/var/log" # systemd journal is stored in /var/log/journal
        "/var/lib/nixos" # user uids and gids
        "/var/lib/systemd/coredump"
      ];
      files = [
        "/etc/machine-id"
      ];

      users.${conUsername} = {
        directories = [
          ".config"
          ".local"
          ".ssh"
          "backups"
          "builds"
          "Documents"
          "Downloads"
          "Games"
          "Music"
          "nixos"
          "Pictures"
          "screencaptures"
          "Sync"
          "Videos"
        ];
      };
    };
  };
}
