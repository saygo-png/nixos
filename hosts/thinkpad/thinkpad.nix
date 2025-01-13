{
  lib,
  pkgs,
  config,
  conHome,
  conUsername,
  conFlakePathRel,
  ...
}: {
  imports = [
    "${conFlakePathRel}/modules/myPulseaudio.nix"
    (
      {config, ...}: {
        options = {
          const = config.constLib.mkConstsFromSet {
            refreshRate = 60;
            screenWidth = 1366;
            screenHeight = 768;
            gaps = 0;
            borderSize = 2;
            accelSpeed = -0.5;
            vsync = true;
            extrasNixosPath = "${conHome}/extrasNixos";
          };
        };
      }
    )
  ];
  # Optimization for ssds
  services.fstrim.enable = true;
  fileSystems."/".options = ["noatime" "nodiratime" "discard"];

  # Wifi
  networking.networkmanager.enable = true;

  # Remove screentearing on x11
  environment.etc."X11/xorg.conf.d/20-intel.conf".text = ''
    Section "Device"
    Identifier "Intel Graphics"
    Driver "i915"
    Option "TearFree" "true"
    EndSection
  '';

  # Function keys
  programs.light.enable = true;
  hardware.acpilight.enable = true;
  services.actkbd = {
    enable = true;
    bindings = [
      {
        # Brightness down
        keys = [224];
        events = ["key"];
        command = "${lib.getExe pkgs.light} -U 1 && notify-send -t 500 $(light)";
      }
      {
        # Brightness up
        keys = [225];
        events = ["key"];
        command = "${lib.getExe pkgs.light} -A 1 && notify-send -t 500 $(light)";
      }
      {
        # Mute
        keys = [113];
        events = ["key"];
        command = "pactl set-sink-mute @DEFAULT_SINK@ toggle && notify-send -t 500 'mute toggled'";
      }
      {
        # Volume down
        keys = [114];
        events = ["key"];
        command = "pactl set-sink-volume @DEFAULT_SINK@ -2% && notify-send -t 500 'lowered volume'";
      }
      {
        # Volume up
        keys = [115];
        events = ["key"];
        command = "pactl set-sink-volume @DEFAULT_SINK@ +2% && notify-send -t 500 'increased volume'";
      }
      # {
      #   # Wifi toggle
      #   keys = [238];
      #   events = ["key"];
      #   command = "notify-send 'toggled wifi'";
      # }
      # {
      #   # Bluetooth toggle
      #   keys = [237];
      #   events = ["key"];
      #   command = "notify-send 'toggled bluetoth'";
      # }
    ];
  };

  # Battery saving.
  boot.kernelParams = ["rcu_nocbs=all" "rcutree.enable_rcu_lazy=1"];
  networking.networkmanager.wifi.powersave = true;
  powerManagement.enable = true;
  services.tlp = {
    enable = true;
    settings = {
      NMI_WATCHDOG = "0";

      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

      CPU_ENERGY_PERF_POLICY_ON_AC = "performance";
      CPU_ENERGY_PERF_POLICY_ON_BAT = "balance_power";

      PLATFORM_PROFILE_ON_AC = "performance";
      PLATFORM_PROFILE_ON_BAT = "balanced";

      CPU_BOOST_ON_AC = 1;
      CPU_BOOST_ON_BAT = 0;
    };
  };

  # Fixes pipewire bug causing the camera to always be on
  # draining battery for no reason.
  # https://gitlab.freedesktop.org/pipewire/pipewire/-/issues/2669
  services.pipewire = {
    wireplumber = {
      enable = lib.mkForce false;
      extraConfig = {
        "10-disable-camera" = {
          "wireplumber.profiles" = {
            main."monitor.libcamera" = "disabled";
          };
        };
      };
    };
  };

  # Hardware decoding.
  environment.sessionVariables = {LIBVA_DRIVER_NAME = "i965";};

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = with pkgs; [
      intel-media-driver
      # intel-vaapi-driver # I think this is added by nixos-hardware
      # libvdpau-va-gl
    ];
  };

  environment.systemPackages = with pkgs; [
    acpi # Cli battery interface.
    powertop # Power drain check

    # Games.
    lutris-free
    winetricks
    wineWowPackages.waylandFull
  ];

  services.libinput.mouse.accelSpeed = lib.strings.floatToString config.const.accelSpeed;
  services.libinput.mouse.accelProfile = lib.mkForce "adaptive";

  # If using hypralnd uncomment
  # for some reason text is much larger on awesomewm
  # and does not need a resize
  stylix.fonts.sizes = {
    popups = 13;
    desktop = 13;
    terminal = 13;
    applications = 14;
  };

  home-manager.users.${conUsername} = {osConfig, ...}: {
    programs.foot.settings.main.pad = lib.mkForce "0x0center";
    programs.alacritty.settings.window.padding = lib.mkForce {
      x = 0;
      y = 0;
    };

    wayland.windowManager.hyprland.settings = {
      general.border_size = lib.mkForce 2;
      animations.enabled = false;
      input.sensitivity = lib.strings.floatToString osConfig.const.accelSpeed;
      input.accel_profile = lib.mkForce config.services.libinput.mouse.accelProfile;

      device = [
        {
          name = "synps/2-synaptics-touchpad";
          enabled = false;
          accel_profile = "adaptive";
          natural_scroll = true;
          disable_while_typing = true;
        }
        {
          name = "tpps/2-elan-trackpoint";
          accel_profile = "adaptive";
        }
      ];
      monitor = [
        ", preferred, auto, 1"
      ];
    };

    programs.nixvim = {
      globals = {
        neovide_padding_top = 0;
        neovide_padding_bottom = 0;
        neovide_padding_right = 0;
        neovide_padding_left = 0;
      };
    };
  };
}
