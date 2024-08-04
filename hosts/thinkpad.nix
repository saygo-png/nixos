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
        keys = [63];
        events = ["key"];
        command = "${lib.getExe pkgs.light} -U 10";
      }
      {
        # Brightness up
        keys = [64];
        events = ["key"];
        command = "${lib.getExe pkgs.light} -A 10";
      }
      {
        # Volume down
        keys = [60];
        events = ["key"];
        command = "${lib.getExe pkgs.pamixer} -d 2 && notify-send -t 500 $(${lib.getExe pkgs.pamixer} --get-volume-human)";
      }
      {
        # Volume up
        keys = [61];
        events = ["key"];
        command = "${lib.getExe pkgs.pamixer} -i 2 && notify-send -t 500 $(${lib.getExe pkgs.pamixer} --get-volume-human)";
      }
      {
        # Mute
        keys = [61];
        events = ["key"];
        command = "${lib.getExe pkgs.pamixer} -t && notify-send -t 500 $(${lib.getExe pkgs.pamixer} --get-volume-human)";
      }
      {
        # Wifi toggle
        keys = [66];
        events = ["key"];
        command = "notify-send 'toggled wifi'";
      }
      {
        # Bluetooth toggle
        keys = [68];
        events = ["key"];
        command = "notify-send 'toggled bluetoth'";
      }
    ];
  };

  # Battery saving.
  powerManagement.enable = true;
  services.thermald.enable = true;
  services.tlp = {
    enable = true;
    settings = {
      #Optional helps save long term battery health
      START_CHARGE_THRESH_BAT0 = 40; # 40 and bellow it starts to charge
      STOP_CHARGE_THRESH_BAT0 = 80; # 80 and above it stops charging
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
