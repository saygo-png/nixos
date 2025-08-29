{
  lib,
  pkgs,
  conUsername,
}: {
  specialisation.gnome.configuration = {
    services.xserver.enable = true;
    services.xserver.displayManager.gdm.enable = true;
    services.xserver.desktopManager.gnome.enable = true;
    services.power-profiles-daemon.enable = lib.mkForce false;
    environment.gnome.excludePackages = with pkgs; [
      orca
      yelp
      geary
      totem
      baobab
      evince
      # loupe
      # sushi
      epiphany
      nautilus
      snapshot
      # sysprof
      # seahorse
      gnome-logs
      gnome-maps
      gnome-music
      simple-scan
      # file-roller
      # gnome-menus
      gnome-console
      gnome-weather
      # gnome-clocks
      gnome-calendar
      gnome-contacts
      gnome-software
      gnome-user-docs
      gnome-calculator
      gnome-characters
      # gnome-bluetooth
      gnome-backgrounds
      gnome-connections
      gnome-font-viewer
      gnome-text-editor
      gnome-disk-utility
      # adwaita-icon-theme
      # gnome-color-manager
      # gnome-control-center
      # gnome-system-monitor
      # nixos-background-info
      # gnome-shell-extensions
      # gnome-shell-extensions
      # glib # for gsettings program
      # gtk3.out # for gtk-launch program
      # xdg-user-dirs-gtk # Used to create the default bookmarks
      gnome-tour # GNOME Shell detects the .desktop file on first log-in.
      # xdg-user-dirs # Update user dirs as described in https://freedesktop.org/wiki/Software/xdg-user-dirs/
    ];

    home-manager.users.${conUsername} = {
      home.sessionVariables.BROWSER = lib.mkForce "firefox";
    };
  };
}
