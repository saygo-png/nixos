{
  lib,
  pkgs,
  config,
  conHome,
  conUsername,
  pkgs-unstable,
  conFlakePathRel,
  conRefresh-rate,
  conScreen-width,
  conScreen-height,
  ...
}: {
  imports = [
    "${conFlakePathRel}/modules/myPipewire.nix"
  ];

  services.libinput.mouse.accelSpeed = "-0.9";
  services.libinput.mouse.accelProfile = lib.mkForce "flat";

  # Optimization for ssds
  services.fstrim.enable = true;
  fileSystems."/".options = ["noatime" "nodiratime" "discard"];

  # Swap super and ctrl
  services.xserver.xkb.options = lib.mkForce "caps:escape,grp:sclk_toggle";

  services.actkbd = {
    enable = true;
    bindings = [
      {
        # Volume down
        keys = [12];
        events = ["key"];
        command = "${lib.getExe pkgs.pamixer} -d 2 && notify-send -t 500 $(${lib.getExe pkgs.pamixer} --get-volume-human)";
      }
      {
        # Volume up
        keys = [13];
        events = ["key"];
        command = "${lib.getExe pkgs.pamixer} -i 2 && notify-send -t 500 $(${lib.getExe pkgs.pamixer} --get-volume-human)";
      }
    ];
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

  # autologin
  services.getty.autologinUser = "${conUsername}";

  home-manager = {
    users.${conUsername} = {
      home = {};

      # THis requires imperative action
      services.easyeffects.enable = true;
      services.easyeffects.preset = "Audio-Technica ATH-M30x";

      wayland.windowManager.hyprland.settings = {
        input.sensitivity = config.services.libinput.mouse.accelSpeed;
        monitor = [
          ", highres@highrr, auto, 1"
        ];
      };

      programs.mangohud = {
        enable = true;
        enableSessionWide = false;
      };

      xdg.configFile."zed/settings.json".text = builtins.toJSON {
        tab_size = 2;
        ensure_final_newline_on_save = false;
        vim_mode = true;
        relative_line_numbers = true;
        ui_font_size = 16;
        auto_update = false;
        use_autoclose = false;
        buffer_font_size = 16;
        show_call_status_icon = false;
        preferred_line_length = 90;
        base_keymap = "VSCode";
        theme = "Gruvbox Dark";
        vim.use_system_clipboard = "always";
        load_direnv = "shell_hook";
        buffer_font_family = "JetBrains Mono";
        git.inline_blame.enabled = false;
        indent_guides = {
          enabled = true;
          line_width = 2;
          active_line_width = 3;
          coloring = "static";
          background_coloring = "disabled";
        };
        inlay_hints.enabled = true;
        journal.hour_format = "hour24";
        telemetry = {
          metrics = false;
          diagnostics = false;
        };
        BINDZ = [
          {
            context = "Workspace";
            bindings = {
              "space f" = "workspace::ToggleLeftDock";
            };
          }
          {
            context = "VimControl && !menu";
            bindings = {
              "space e" = "editor::Hover";
            };
          }
          {
            context = "Editor && vim_mode == normal && !VimWaiting && !menu";
            bindings = {
              shift-q = "pane::CloseActiveItem";
              "space c" = "editor::Format";
              "space c a" = "editor::ToggleCodeActions";
              "shift-b" = "pane::ActivatePrevItem";
            };
          }
          {
            context = "Editor && vim_mode == insert && !VimWaiting && !menu";
            bindings = {
              enter = "editor::SelectLargerSyntaxNode";
              ctrl-v = "editor::Paste"; # vim default: visual block mode
              ctrl-c = "editor::Copy"; # vim default: return to normal mode
            };
          }
          {
            context = "Editor && !VimWaiting && !menu";
            bindings = {
              "cmd-[" = "pane::GoBack";
              "cmd-]" = "pane::GoForward";
            };
          }
          {
            # Vim File Tree ("ProjectPanel") actions
            context = "ProjectPanel && not_editing";
            bindings = {
              "h" = "project_panel::CollapseSelectedEntry";
              "l" = "project_panel::ExpandSelectedEntry";
              "j" = "menu::SelectNext";
              "k" = "menu::SelectPrev";
              "o" = "menu::Confirm";
              "r" = "project_panel::Rename";
              "z c" = "project_panel::CollapseSelectedEntry";
              "z o" = "project_panel::ExpandSelectedEntry";
              "shift-o" = "project_panel::RevealInFinder";
              "x" = "project_panel::Cut";
              "c" = "project_panel::Copy";
              "p" = "project_panel::Paste";
              "d" = "project_panel::Delete";
              "a" = "project_panel::NewFile";
              "shift-a" = "project_panel::NewDirectory";
              "shift-y" = "project_panel::CopyRelativePath";
              "g y" = "project_panel::CopyPath";
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

  # There is a module for this but i find nix -> yaml weird
  services.borgmatic.enable = true;
  environment.etc = {
    "borgmatic/config.yaml" = {
      mode = "0777";
      text = ''
        before_backup:
        - findmnt /media/backup > /dev/null || exit 75
        keep_daily: 1
        keep_monthly: 12
        keep_weekly: 4
        keep_yearly: 10
        exclude_caches: true
        exclude_patterns:
        - '*/.cache'
        - '*/.npm'
        - '*/.direnv'
        - '*/.devenv'
        - '*/.devenv*'
        - '*/clj-kondo'
        - '*/__pycache__
        - '*/venv.bak
        - '*/env.bak
        - '*/node_modules'
        - '*/.local/share/Steam'
        - '*/Games/battlenet'
        repositories:
        - path: /media/backup/backup.borg
        source_directories:
        - ${conHome}
      '';
    };
  };

  environment.systemPackages = with pkgs; [
    borgbackup
    pkgs-unstable.zed-editor # Another text editor
    # Game launcher
    lutris
    winetricks
    jdk8 # Java 8 for minecraft
    wineWowPackages.waylandFull
    blender

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
    extraCompatPackages = [pkgs.proton-ge-bin];
    extraPackages = [
      pkgs.libpng
      pkgs.libkrb5
      pkgs.gamemode
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
    capSysNice = true; # Breaks it inside of hyprland
  };
}
