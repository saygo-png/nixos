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
      wayland.windowManager.hyprland.settings = {
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
