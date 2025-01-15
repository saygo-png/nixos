{
  lib,
  pkgs,
  config,
  conHome,
  conUsername,
  pkgs-unstable,
  ...
}: {
  imports =
    [
      ({config, ...}: {
        options = {
          const = config.constLib.mkConstsFromSet {
            refreshRate = 144;
            screenWidth = 1920;
            screenHeight = 1080;
            gaps = 6;
            borderSize = 1;
            accelSpeed = -0.9;
            vsync = false;
            extrasNixosPath = "${conHome}/extrasNixos";
          };
        };
      })
    ]
    ++ lib.my.withModules [
      "myPipewire.nix"
      "myMullvad.nix"
      "myRocm.nix"
    ];

  services.libinput.mouse.accelSpeed = lib.strings.floatToString config.const.accelSpeed;
  services.libinput.mouse.accelProfile = lib.mkForce "flat";

  # Open ports for qbittorrent
  networking.firewall.allowedTCPPorts = [39578];
  networking.firewall.allowedUDPPorts = [39578];
  services.fail2ban.enable = true;

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

  # Gaming.
  # services.xserver.deviceSection = ''
  #   Option "TearFree" "True"
  # '';

  # Change cpu governor to performance for increased performance.
  powerManagement.cpuFreqGovernor = "performance";
  boot.kernelPackages = pkgs.linuxPackages_xanmod_latest;

  # Taken from https://github.com/fufexan/nix-gaming
  boot.kernel.sysctl = {
    # 20-shed.conf
    "kernel.sched_cfs_bandwidth_slice_us" = 3000;
    # 20-net-timeout.conf
    # This is required due to some games being unable to reuse their TCP ports
    # if they're killed and restarted quickly - the default timeout is too large.
    "net.ipv4.tcp_fin_timeout" = 5;
    # 30-vm.conf
    # USE MAX_INT - MAPCOUNT_ELF_CORE_MARGIN.
    # see comment in include/linux/mm.h in the kernel tree.
    "vm.max_map_count" = 2147483642;
  };

  services.xserver.videoDrivers = ["modesetting"];

  hardware = {
    amdgpu = {
      amdvlk.enable = false;
      initrd.enable = true;
      opencl.enable = true;
    };
    graphics = {
      enable = true;
      enable32Bit = true;
      extraPackages = with pkgs; [
        libdrm
        libva
        vaapiVdpau
        libvdpau-va-gl
      ];
      extraPackages32 = with pkgs; [
        libva
        libvdpau-va-gl
      ];
    };
  };

  # autologin
  services.getty.autologinUser = "${conUsername}";

  home-manager.users.${conUsername} = _: {
    # This requires imperative action
    # services.easyeffects.enable = true;
    # services.easyeffects.preset = "Audio-Technica ATH-M30x";

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

  # Allowed unfree packages
  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [
      "steam"
      "steam-run"
      "steam-original"
      "steam-unwrapped"
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
        - '*/__pycache__'
        - '*/venv.bak'
        - '*/env.bak'
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
  services.ratbagd.enable = true; # For piper
  environment.systemPackages = with pkgs; [
    piper # For ratbagd
    borgbackup
    pkgs-unstable.zed-editor.fhs # Another text editor

    # Game launcher
    lutris
    winetricks
    wineWowPackages.waylandFull
    blender-hip

    (
      writeShellScriptBin
      "sgamescope" # [s]team [gamescope]
      
      ''
        gamescope \
          -w ${builtins.toString config.const.screenWidth} \
          -W ${builtins.toString config.const.screenWidth} \
          -h ${builtins.toString config.const.screenHeight} \
          -H ${builtins.toString config.const.screenHeight} \
          -r ${builtins.toString config.const.refreshRate} -f steam
      ''
    )
  ];

  # # This is the command for running all 3 programs at once that u put into steam
  # # gamemoderun gamescope -w 1920 -h 1080 -f -- mangohud %command%
  programs.steam = {
    enable = true;
    gamescopeSession.enable = true;
    # Load the extest library into Steam, to translate X11 input events to uinput events (for using Steam Input on Wayland).
    # extest.enable = true;
    extraCompatPackages = [pkgs-unstable.proton-ge-bin];
    extraPackages = [
      pkgs.gamescope
      pkgs.gamemode
      # pkgs.libpng
      # pkgs.libkrb5
      # pkgs.keyutils
      # pkgs.libvorbis
      # pkgs.xorg.libXi
      # pkgs.libpulseaudio
      # pkgs.xorg.libXcursor
      # pkgs.stdenv.cc.cc.lib
      # pkgs.xorg.libXinerama
      # pkgs.xorg.libXScrnSaver
    ];
  };
  programs.gamemode.enable = true;
  programs.gamescope = {
    enable = true;
    capSysNice = true; # Breaks it inside of hyprland
  };
}
