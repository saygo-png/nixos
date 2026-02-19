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
      (
        {config, ...}: {
          options = {
            const = config.constLib.mkConstsFromSet {
              host = "thinkpad";
              refreshRate = 60;
              screenWidth = 1366;
              screenHeight = 768;
              gaps = 0;
              borderSize = 2;
              accelSpeed = -0.5;
              vsync = true;
              extrasNixosPath = "${conHome}/extrasNixos";
              flakePath = "${conHome}/nixos";
            };
          };
        }
      )
    ]
    ++ lib.my.withModules [
      "pulseaudio.nix"
      # "mullvad.nix"
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

  networking.hostId = "91b1153b";

  # Wifi
  networking.networkmanager.enable = true;

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
  services.upower.enable = true;
  boot.kernelParams = ["rcu_nocbs=all" "rcutree.enable_rcu_lazy=1"];
  networking.networkmanager.wifi.powersave = true;
  powerManagement.enable = true;
  services.tlp = {
    enable = true;
    settings = {
      NMI_WATCHDOG = "0";
      WIFI_PWR_ON_BAT = "on";
      CPU_DRIVER_OPMODE_ON_BAT = "passive";

      START_CHARGE_THRESH_BAT0 = 75;
      STOP_CHARGE_THRESH_BAT0 = 80;

      START_CHARGE_THRESH_BAT1 = 75;
      STOP_CHARGE_THRESH_BAT1 = 80;

      # This is only for ACTIVE OPMODE
      # CPU_SCALING_GOVERNOR_ON_AC = "performance";
      # CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

      CPU_ENERGY_PERF_POLICY_ON_AC = "performance";
      CPU_ENERGY_PERF_POLICY_ON_BAT = "balance_power";

      CPU_MIN_PERF_ON_BAT = 0;
      CPU_MAX_PERF_ON_BAT = 50;

      PLATFORM_PROFILE_ON_AC = "performance";
      PLATFORM_PROFILE_ON_BAT = "balanced";

      CPU_BOOST_ON_AC = 1;
      CPU_BOOST_ON_BAT = 0;

      CPU_HWP_DYN_BOOST_ON_AC = 1;
      CPU_HWP_DYN_BOOST_ON_BAT = 0;
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
  ];

  services.libinput.mouse.accelSpeed = lib.strings.floatToString config.const.accelSpeed;
  services.libinput.mouse.accelProfile = lib.mkForce "adaptive";

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
  };
}
