{
  conUsername,
  pkgs,
  lib,
  ...
}: {
  environment.systemPackages = [
    (lib.my.mkChromiumKiosk pkgs {
      name = "VIA-kiosk";
      url = "https://usevia.app";
      desktopName = "VIA configurator kiosk";
      comment = "Launch the VIA app in fullscreen kiosk mode";
      flags = ["--password-store=basic"]; # Hide stupid keyring popup
    })
  ];

  # This lets the VIA webapp find the keyboard.
  services.udev = {
    extraRules = ''
      KERNEL=="hidraw*", SUBSYSTEM=="hidraw", GROUP="hidraw", MODE="0660", TAG+="uaccess", TAG+="udev-acl"
    '';
  };
  users.groups.hidraw = {};
  users.users.${conUsername}.extraGroups = ["hidraw"];
}
