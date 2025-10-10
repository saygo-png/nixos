{
  pkgs,
  config,
  conUsername,
  ...
}: {
  custom.persist.home = {
    directories = [".local/share/lutris"];
    cache.directories = [
      ".cache/lutris"

      ".cache/radv_builtin_shaders"
      ".cache/mesa_shader_cache_db"
      ".cache/mesa_shader_cache"
    ];
  };

  # Allowed unfree packages
  custom.allowedUnfreePkgs = [
    pkgs.steam
    pkgs.steam-run
    pkgs.steam-unwrapped
  ];

  # This fixes lag in Saints Row: The Third
  # game tries to connect to a dead server which freezes the game for a bit
  networking.extraHosts = ''
    127.0.0.1 sr3.hydra.agoragames.com
  '';

  home-manager.users.${conUsername} = {
    config,
    lib,
    ...
  }: {
    home.activation.copyConfigFilesToSteamEnv = lib.hm.dag.entryAfter ["writeBoundary"] ''
      # Copy files to steam directory
      set +e
      ${lib.concatStringsSep "\n" (lib.mapAttrsToList (
          path: file: let
            prefix = "/home/${conUsername}/";
            steamPath = prefix + ".local/share/steam-home/" + (lib.removePrefix prefix path);
          in ''
            run mkdir -p "$(dirname "${steamPath}")" || true
            run ln -sf "${file.source}" "${steamPath}" || true
          ''
        )
        config.home.file)}
    '';

    programs.mangohud = {
      enable = true;
      enableSessionWide = false;
    };
  };

  programs = {
    gamemode.enable = true;
    steam = {
      enable = true;
      gamescopeSession.enable = true;
      extraCompatPackages = [pkgs.proton-ge-bin];
      extraPackages = [pkgs.gamemode];
    };
  };

  # Taken from https://github.com/fufexan/nix-gaming
  boot.kernel.sysctl = {
    # 20-shed.conf
    "kernel.sched_cfs_bandwidth_slice_us" = 3000;
    # 20-net-timeout.conf
    # This is required due to some games being unable to reuse their TCP ports
    # if they're killed and restarted quickly - the default timeout is too large.
    "net.ipv4.tcp_fin_timeout" = 5;
    # 30-splitlock.conf
    # Prevents intentional slowdowns in case games experience split locks
    # This is valid for kernels v6.0+
    "kernel.split_lock_mitigate" = 0;
    # 30-vm.conf
    # USE MAX_INT - MAPCOUNT_ELF_CORE_MARGIN.
    # see comment in include/linux/mm.h in the kernel tree.
    "vm.max_map_count" = 2147483642;
  };
  environment.systemPackages = [
    # Game launcher
    pkgs.lutris
    pkgs.winetricks
    pkgs.wineWowPackages.waylandFull
    pkgs.steam-run
  ];
}
