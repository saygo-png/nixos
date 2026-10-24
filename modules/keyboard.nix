{
  pkgs,
  lib,
  conUsername,
  ...
}: let
  vendor = "8888";
  product = "2222";
  interface = "01";

  browser = pkgs.writeShellApplication {
    name = "via-kiosk-browser";
    text = ''
      XDG_RUNTIME_DIR="/run/user/$(id -u)"
      export XDG_RUNTIME_DIR
      # For the desktop's file picker (xdg-desktop-portal), to load JSON definitions.
      export DBUS_SESSION_BUS_ADDRESS="unix:path=$XDG_RUNTIME_DIR/bus"
      exec ${lib.getExe pkgs.ungoogled-chromium} \
        --ozone-platform=wayland \
        --user-data-dir="$HOME/profile" \
        --no-first-run --no-default-browser-check \
        --password-store=basic \
        --app=https://usevia.app
    '';
  };

  # Opens VIA on the Wayland display this is started from.
  launcher = pkgs.writeShellApplication {
    name = "via-kiosk";
    text = ''systemctl start "via-kiosk@$WAYLAND_DISPLAY.service"'';
  };
in {
  users.groups.via = {};

  services.udev.extraRules = ''
    SUBSYSTEM=="hidraw", ENV{ID_VENDOR_ID}=="${vendor}", ENV{ID_MODEL_ID}=="${product}", ENV{ID_USB_INTERFACE_NUM}=="${interface}", GROUP="via", MODE="0660"
  '';

  # The instance is the Wayland socket to show the window on.
  systemd.services."via-kiosk@" = {
    description = "VIA keyboard configurator on %i";
    serviceConfig = {
      User = conUsername;
      SupplementaryGroups = ["via"];
      ExecStart = lib.getExe browser;
      # The browser's home and profile live in a /tmp only this service can
      # see, so nothing running as you can plant settings, extensions or
      # certificates in them.
      PrivateTmp = true;
      Environment = [
        "WAYLAND_DISPLAY=%i"
        "HOME=/tmp/home"
      ];
      ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p /tmp/home";
    };
  };

  environment.systemPackages = [
    launcher
    (pkgs.makeDesktopItem {
      name = "via-kiosk";
      desktopName = "VIA configurator";
      comment = "Configure the keyboard with VIA";
      exec = lib.getExe launcher;
    })
  ];
}
