{
  lib,
  pkgs,
  config,
  conHome,
  conUsername,
  ...
}: {
  imports =
    [
      ({config, ...}: {
        options = {
          const = config.constLib.mkConstsFromSet {
            host = "pc";
            refreshRate = 144;
            screenWidth = 1920;
            screenHeight = 1080;
            gaps = 0;
            borderSize = 2;
            accelSpeed = -0.9;
            vsync = false;
            extrasNixosPath = "${conHome}/builds/extrasNixos";
            flakePath = "${conHome}/nixos";
          };
        };
      })
    ]
    ++ lib.my.withModules [
      "myPipewire.nix"
      "myRocm.nix"
    ];

  services.sanoid = {
    enable = true;
    datasets = {
      "zroot/local/persist" = {
        hourly = 50;
        daily = 15;
        weekly = 3;
        monthly = 1;
      };
    };
  };

  services.libinput.mouse.accelSpeed = lib.strings.floatToString config.const.accelSpeed;
  services.libinput.mouse.accelProfile = lib.mkForce "flat";

  networking.hostId = "a4e735aa";

  # Open ports for qbittorrent
  networking.firewall.allowedTCPPorts = [39578];
  networking.firewall.allowedUDPPorts = [39578];
  services.fail2ban.enable = true;

  # Caps = escape, keyboard change key
  services.xserver.xkb.options = lib.mkForce "caps:escape,grp:sclk_toggle";

  # Gaming.
  services.xserver.xrandrHeads = [
    {
      output = "HDMI-2";
      monitorConfig = ''
        Modeline "1280x720_144.00"  198.75  1280 1384 1520 1760  720 723 728 786 -hsync +vsync
      '';
    }
  ];

  # https://gitlab.freedesktop.org/drm/amd/-/issues/1500
  services.udev.extraRules = ''
    KERNEL=="card1", SUBSYSTEM=="drm", DRIVERS=="amdgpu", ATTR{device/power_dpm_force_performance_level}="manual", ATTR{device/pp_power_profile_mode}="1"
  '';

  services.xserver.deviceSection = ''
    Option "TearFree" "True"
  '';

  # Change cpu governor to performance for increased performance.
  powerManagement.cpuFreqGovernor = "performance";
  boot.kernelPackages = pkgs.linuxPackages_xanmod_latest;

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

  services.borgmatic = {
    enable = true;
    configurations.persist = {
      repositories = [
        {
          path = "/media/backup/backup.borg";
          label = "local";
        }
      ];
      exclude_caches = true;
      source_directories = ["/persist"];
      commands = [
        {
          before = "repository";
          run = ["findmnt /media/backup > /dev/null || exit 75"];
        }
      ];
      keep_monthly = -1;
    };
  };

  services.ratbagd.enable = true; # For piper
  environment.systemPackages = [
    pkgs.piper # For ratbagd
    pkgs.blender-hip
  ];
}
