{
  lib,
  pkgs,
  conUsername,
  ...
}: {
  custom.persist.home = {
    directories = [
      ".local/share/lutris"
      ".local/share/applications" # For lutris menu entries
    ];
    cache.directories = [
      ".cache/lutris"

      ".cache/radv_builtin_shaders"
      ".cache/mesa_shader_cache_db"
      ".cache/mesa_shader_cache"
    ];
  };

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


  # I only use this to play Diablo 3 sometimes, so make it lazy.
  custom.lazyPackages = let
    proton = pkgs.proton-ge-bin.steamcompattool;
  in [
    (pkgs.writeShellApplication {
      name = "lutris";
      text = ''
        runners="''${XDG_DATA_HOME:-$HOME/.local/share}/lutris/runners/wine"
        mkdir -p "$runners"
        ln -sfn ${proton} "$runners/${lib.toLower proton.name}"
        exec ${lib.getExe (pkgs.lutris.override {steamSupport = false;})} "$@"
      '';
    })
  ];

  nixpkgs.overlays = [
    (_: prev: {
      gamescope = prev.gamescope.overrideAttrs (_: {
        # https://github.com/ValveSoftware/gamescope/issues/1924#issuecomment-3725667842
        NIX_CFLAGS_COMPILE = ["-fno-fast-math"];
      });
    })
  ];

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
}
