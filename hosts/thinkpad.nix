{
  lib,
  host,
  pkgs,
  config,
  inputs,
  conHome,
  conUsername,
  conFlake-path,
  pkgs-unstable,
  conAccentColor,
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

  programs.light.enable = true;
  services.actkbd = {
    enable = true;
    bindings = [
      {
        keys = [224];
        events = ["key"];
        command = "/run/current-system/sw/bin/light -A 10";
      }
      {
        keys = [225];
        events = ["key"];
        command = "/run/current-system/sw/bin/light -U 10";
      }
    ];
  };

  # Battery saving.
  powerManagement.enable = true;
  services.thermald.enable = true;
  services.tlp.enable = true;
  hardware.opengl = {
    enable = true;
    driSupport = true;
  };

  environment.systemPackages = with pkgs; [acpi];

  services.libinput.mouse.accelSpeed = "-0.9";
  home-manager = {
    users.${conUsername} = {
      home = {};
      # Media controls for bluetooth headphones
      services.mpris-proxy.enable = true;

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
            # accel_profile = "flat";
            # natural_scroll = true;
            # disable_while_typing = true;
          }
          {
            name = "tpps/2-elan-trackpoint";
            accel_profile = "flat";
          }
        ];
      };
    };
  };
}
