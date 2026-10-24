{
  lib,
  pkgs,
  config,
  conHome,
  conUsername,
  ...
}: {
  imports = lib.my.withModules [
    "pulseaudio.nix"
    # "mullvad.nix"
  ];

  networking.hostName = "thinkpad";
  const = {
    refreshRate = 60;
    screenWidth = 1366;
    screenHeight = 768;
    gaps = 0;
    borderSize = 2;
    accelSpeed = -0.5;
    vsync = true;
    extrasNixosPath = "${conHome}/extrasNixos";
  };

  networking.hostId = "91b1153b";

  # Wifi
  networking.networkmanager.enable = true;

  # Function keys. Brightness keys are bound in niri.kdl to brightnessctl.
  services.actkbd = {
    enable = true;
    bindings = [
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

  # Hardware decoding.
  environment.sessionVariables = {LIBVA_DRIVER_NAME = "i965";};

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  environment.systemPackages = [
    pkgs.acpi # Cli battery interface.
    pkgs.brightnessctl # Backlight control, works through logind
    pkgs.powertop # Power drain check
    pkgs.vlock
  ];

  services.libinput.mouse.accelSpeed = lib.strings.floatToString config.const.accelSpeed;
  services.libinput.mouse.accelProfile = lib.mkForce "adaptive";

  home-manager.users.${conUsername} = {
    home.shellAliases = {
      "lock" = "sudo vlock -nas";
    };
    programs.alacritty.settings.window.padding = lib.mkForce {
      x = 0;
      y = 0;
    };
  };
}
