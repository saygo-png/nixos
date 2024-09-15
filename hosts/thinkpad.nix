{
  lib,
  # host,
  pkgs,
  config,
  # inputs,
  # conHome,
  conUsername,
  # conFlake-path,
  # pkgs-unstable,
  # conAccentColor,
  conRefresh-rate,
  conScreen-width,
  conScreen-height,
  ...
}: {
  # Bluetooth
  services.blueman.enable = true;
  hardware.bluetooth = {
    enable = true; # enables support for Bluetooth
    powerOnBoot = true; # powers up the default Bluetooth controller on boot
  };

  # Wifi
  networking.networkmanager.enable = true;

  # Function keys
  programs.light.enable = true;
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
        command = "${lib.getExe pkgs.pamixer} -t && notify-send -t 500 $(${lib.getExe pkgs.pamixer} --get-volume-human)";
      }
      {
        # Volume down
        keys = [114];
        events = ["key"];
        command = "${lib.getExe pkgs.pamixer} -d 2 && notify-send -t 500 $(${lib.getExe pkgs.pamixer} --get-volume-human)";
      }
      {
        # Volume up
        keys = [115];
        events = ["key"];
        command = "${lib.getExe pkgs.pamixer} -i 2 && notify-send -t 500 $(${lib.getExe pkgs.pamixer} --get-volume-human)";
      }
      {
        # Wifi toggle
        keys = [238];
        events = ["key"];
        command = "notify-send 'toggled wifi'";
      }
      {
        # Bluetooth toggle
        keys = [237];
        events = ["key"];
        command = "notify-send 'toggled bluetoth'";
      }
    ];
  };

  # Battery saving.
  networking.networkmanager.wifi.powersave = true;
  powerManagement.enable = true;
  services.tlp.enable = true;

  # On battery ur cpu will go down to 400 freq if this is off
  # Still does after pluggin in cable and unplugging :(
  # TODO fix
  services.throttled.enable = true;

  # Fixes pipewire bug causing the camera to always be on
  # draining battery for no reason.
  # https://gitlab.freedesktop.org/pipewire/pipewire/-/issues/2669
  services.pipewire = {
    wireplumber = {
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
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [intel-vaapi-driver libvdpau-va-gl];
  };

  environment.systemPackages = with pkgs; [
    acpi # Cli battery interface.
    powertop # Power drain check

    # Games.
    lutris-free
    winetricks
    wineWowPackages.waylandFull
  ];

  services.libinput.mouse.accelSpeed = "1";
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

  home-manager = {
    users.${conUsername} = {
      # Media controls for bluetooth headphones
      services.mpris-proxy.enable = true;

      programs.foot.settings.main.pad = lib.mkForce "0x0center";
      programs.alacritty.settings.window.padding = lib.mkForce {
        x = 0;
        y = 0;
      };

      wayland.windowManager.hyprland.settings = {
        general.border_size = lib.mkForce 2;
        animations.enabled = false;
        input.sensitivity = config.services.libinput.mouse.accelSpeed;
        input.accel_profile = lib.mkForce "adaptive";

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
        monitor = let
          res = lib.concatStrings [
            "${builtins.toString conScreen-width}x"
            "${builtins.toString conScreen-height}@"
            "${builtins.toString conRefresh-rate}"
          ];
        in [
          # "eDP-1, ${res}, 0x0, 1"
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
  };
}
