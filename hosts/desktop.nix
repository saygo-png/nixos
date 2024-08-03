{
  lib,
  host,
  pkgs,
  inputs,
  conHome,
  conUsername,
  conFlake-path,
  pkgs-unstable,
  conAccentColor,
  conRefresh-rate,
  conScreen-width,
  conScreen-height,
  ...
}: {
  services.libinput.enable = true;
  services.libinput.mouse.accelSpeed = "-0.9";

  # X11 window manager for games
  services.xserver.windowManager.awesome = {
    enable = true;
  };

  # Change cpu governor to performance for increased performance.
  powerManagement.cpuFreqGovernor = "performance";
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      vaapiVdpau
      libvdpau-va-gl
      rocm-opencl-icd
      rocm-opencl-runtime
    ];
    extraPackages32 = with pkgs.pkgsi686Linux; [libva];
  };
  services.xserver.videoDrivers = ["amdgpu"];

  home-manager = {
    users.${conUsername} = {
      home = {
      };
      wayland.windowManager.hyprland.settings.input.sensitivity = -0.9;
      programs.mangohud = {
        enable = true;
        enableSessionWide = false;
      };
      xdg.configFile."zed/settings.json".text = builtins.toJSON {
        tab_size = 2;
        vim_mode = true;
        ui_font_size = 16;
        auto_update = false;
        buffer_font_size = 16;
        base_keymap = "VSCode";
        theme = "Gruvbox Dark";
        vim.use_system_clipboard = "always";
        buffer_font_family = "JetBrains Mono";
        inlay_hints = {
          enabled = true;
          edit_debounce_ms = 700;
          show_type_hints = true;
          scroll_debounce_ms = 50;
          show_other_hints = true;
          show_parameter_hints = true;
        };
        journal = {
          hour_format = "hour24";
        };
        telemetry = {
          metrics = false;
          diagnostics = false;
        };
        BINDZ = [
          {
            context = "Editor && !VimWaiting && !menu";
            bindings = {
              ctrl-y = "editor::Undo"; # vim default: line up
              ctrl-o = "workspace::Open"; # vim default: go back
              ctrl-a = "editor::SelectAll"; # vim default: increment
              ctrl-v = "editor::Paste"; # vim default: visual block mode
              ctrl-c = "editor::Copy"; # vim default: return to normal mode
            };
          }
        ];
      };
    };
  };

  # Allowed unfree packages
  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [
      "steam"
      "steam-run"
      "steam-original"
    ];

  environment.systemPackages = with pkgs; [
    zed-editor # Another text editor
    # Game launcher
    lutris
    winetricks
    jdk8 # Java 8 for minecraft
    wineWowPackages.waylandFull

    (writeShellScriptBin
      "sgamescope" # [s]team [gamescope]

      ''
        gamescope -w ${builtins.toString conScreen-width} -W ${builtins.toString conScreen-width} -h ${builtins.toString conScreen-height} -H ${builtins.toString conScreen-height} -r ${builtins.toString conRefresh-rate} -f steam
      '')
  ];

  # # This is the command for running all 3 programs at once that u put into steam
  # # gamemoderun gamescope -w 1920 -h 1080 -f -- mangohud %command%
  programs.steam = {
    enable = true;
    gamescopeSession.enable = true;
    # Load the extest library into Steam, to translate X11 input events to uinput events (for using Steam Input on Wayland).
    # extest.enable = true;
    extraCompatPackages = [
      pkgs-unstable.proton-ge-bin
    ];
    extraPackages = [
      pkgs.libpng
      pkgs.libkrb5
      pkgs.libkrb5
      pkgs.gamemode
      pkgs.keyutils
      pkgs.keyutils
      pkgs.gamescope
      pkgs.libvorbis
      pkgs.xorg.libXi
      pkgs.libpulseaudio
      pkgs.xorg.libXcursor
      pkgs.stdenv.cc.cc.lib
      pkgs.xorg.libXinerama
      pkgs.xorg.libXScrnSaver
    ];
  };
  programs.gamemode.enable = true;
  programs.gamescope = {
    enable = true;
    capSysNice = true;
  };
}
