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
  # Battery saving.
  powerManagement.enable = true;
  services.thermald.enable = true;
  services.tlp.enable = true;
  hardware.opengl = {
    enable = true;
    driSupport = true;
  };

  home-manager = {
    users.${conUsername} = {
      home = {};
      wayland.windowManager.hyprland.settings = {
        device = [
          {
            name = "synps/2-synaptics-touchpad";
            enabled = false;

            sensitivity = 0.75;
            accel_profile = "flat";
            natural_scroll = true;
            disable_while_typing = true;
          }
          {
            name = "tpps/2-elan-trackpoint";
            sensitivity = 0.5;
            accel_profile = "flat";
          }
        ];
      };
    };
  };
}
