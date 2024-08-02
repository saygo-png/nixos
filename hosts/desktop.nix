{
  pkgs,
  lib,
  inputs,
  host,
  pkgs-unstable,
  ...
}: let
  constants = {
    refresh-rate = 144;
    screen-width = 1920;
    screen-height = 1080;
  };
in {
  imports = [
    inputs.home-manager.nixosModules.default
  ];

  # Allowed unfree packages
  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [
      "steam"
      "steam-original"
      "steam-run"
    ];

  services.libinput.enable = true;
  services.libinput.mouse.accelSpeed = "-0.9";

  environment.systemPackages = with pkgs; [
    zed-editor # Another text editor
    # Game launcher
    lutris
    wineWowPackages.waylandFull
    winetricks
    jdk8 # Java 8 for minecraft

    (writeShellScriptBin
      "sgamescope" # [s]team [gamescope]

      ''
        gamescope -w ${builtins.toString constants.screen-width} -W ${builtins.toString constants.screen-width} -h ${builtins.toString constants.screen-height} -H ${builtins.toString constants.screen-height} -r ${builtins.toString constants.refresh-rate} -f steam
      '')
  ];

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

  home = {
    wayland.windowManager.hyprland.settings.input.sensitivity = -0.9;
  };

  # File synchronization.
  services.syncthing = {
    enable = true;
    settings.options.relaysEnabled = false;
    openDefaultPorts = true;
    overrideFolders = false;
    overrideDevices = false;
    user = constants.username;
    dataDir = constants.home;
    settings.devices = {
      phone = {
        addresses = [
          "tcp://192.168.1.10:22000"
        ];
        id = "Z7AOC2O-CYXT6XV-Y67O5RB-VAXE2JT-JV36AMW-KWQ3U6Z-PVTINXB-IQ2UHQ7";
        autoAcceptFolders = true;
      };
    };
  };

  # This is the command for running all 3 programs at once that u put into steam
  # gamemoderun gamescope -w 1920 -h 1080 -f -- mangohud %command%
  programs.steam = {
    enable = true;
    gamescopeSession.enable = true;
    # Load the extest library into Steam, to translate X11 input events to uinput events (for using Steam Input on Wayland).
    # extest.enable = true;
    extraCompatPackages = [
      pkgs-unstable.proton-ge-bin
    ];
    extraPackages = [
      pkgs.gamescope
      pkgs.gamemode
      pkgs.libkrb5
      pkgs.keyutils
      pkgs.xorg.libXcursor
      pkgs.xorg.libXi
      pkgs.xorg.libXinerama
      pkgs.xorg.libXScrnSaver
      pkgs.libpng
      pkgs.libpulseaudio
      pkgs.libvorbis
      pkgs.stdenv.cc.cc.lib
      pkgs.libkrb5
      pkgs.keyutils
    ];
  };

  programs.gamemode.enable = true;

  programs.mangohud = {
    enable = true;
    enableSessionWide = false;
  };
  programs.gamescope = {
    enable = true;
    capSysNice = true;
  };

  xdg.configFile."zed/settings.json".text = builtins.toJSON {
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
      BINDZ = [
        {
          context = "Editor && !VimWaiting && !menu";
          bindings = {
            ctrl-c = "editor::Copy"; # vim default: return to normal mode
            ctrl-v = "editor::Paste"; # vim default: visual block mode
            ctrl-y = "editor::Undo"; # vim default: line up
            ctrl-o = "workspace::Open"; # vim default: go back
            ctrl-a = "editor::SelectAll"; # vim default: increment
          };
        }
      ];
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
    };
    tab_size = 2;
    vim_mode = true;
    ui_font_size = 16;
    auto_update = false;
    buffer_font_size = 16;
    base_keymap = "VSCode";
    theme = "Gruvbox Dark";
    vim.use_system_clipboard = "always";
    buffer_font_family = "JetBrains Mono";
    BINDZ = [
      {
        context = "Editor && !VimWaiting && !menu";
        bindings = {
          ctrl-c = "editor::Copy"; # vim default: return to normal mode
          ctrl-v = "editor::Paste"; # vim default: visual block mode
          ctrl-y = "editor::Undo"; # vim default: line up
          ctrl-o = "workspace::Open"; # vim default: go back
          ctrl-a = "editor::SelectAll"; # vim default: increment
        };
      }
    ];
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
  };
}
