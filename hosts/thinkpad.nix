{
  lib,
  # host,
  pkgs,
  # config,
  # inputs,
  # conHome,
  conUsername,
  # conFlake-path,
  # pkgs-unstable,
  # conAccentColor,
  # conRefresh-rate,
  # conScreen-width,
  # conScreen-height,
  ...
}: {
  # Bluetooth
  services.blueman.enable = true;
  hardware.bluetooth = {
    enable = true; # enables support for Bluetooth
    powerOnBoot = true; # powers up the default Bluetooth controller on boot
  };

  # Function keys
  programs.light.enable = true;
  services.actkbd = {
    enable = true;
    bindings = [
      {
        # Brightness down
        keys = [224];
        events = ["key"];
        command = "${lib.getExe pkgs.light} -U 1";
      }
      {
        # Brightness up
        keys = [225];
        events = ["key"];
        command = "${lib.getExe pkgs.light} -A 1";
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
    # Cli battery interface.
    acpi
    # Power drain check
    powertop
    # Games.
    lutris-free
    winetricks
    wineWowPackages.waylandFull
  ];

  services.libinput.mouse.accelSpeed = "-0.9";
  home-manager = {
    users.${conUsername} = {
      home = {
        sessionVariables = {
          TERMINAL = lib.mkForce ''foot'';
        };
      };

      # Media controls for bluetooth headphones
      services.mpris-proxy.enable = true;

      programs.foot.settings.main.pad = lib.mkForce "0x0center";
      programs.alacritty.settings.window.padding = lib.mkForce {
        x = 0;
        y = 0;
      };

      wayland.windowManager.hyprland.settings = let
        gaps_in = 0;
        gaps_out = 0;
      in {
        general = {
          gaps_in = lib.mkForce gaps_in;
          gaps_out = lib.mkForce gaps_out;
          border_size = lib.mkForce 2;
        };
        device = [
          {
            name = "synps/2-synaptics-touchpad";
            enabled = false;
            accel_profile = "flat";
            natural_scroll = true;
            disable_while_typing = true;
          }
          {
            name = "tpps/2-elan-trackpoint";
            accel_profile = "flat";
          }
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
