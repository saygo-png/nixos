{
  lib,
  pkgs,
  conHome,
  conUsername,
  ...
}: {
  environment.systemPackages = with pkgs; [
    kdePackages.qtsvg # Icons for dolphin
    kdePackages.qtwayland # qt6
    libsForQt5.qt5.qtwayland

    # fix kirigami apps look
    # for example in filelight, without it the app looks weird
    # https://github.com/NixOS/nixpkgs/pull/202990#issuecomment-1328068486
    kdePackages.qqc2-desktop-style # qt6
    libsForQt5.qqc2-desktop-style
  ];
  home-manager.users.${conUsername} = {config, ...}: {
    qt = {
      enable = true;
      style.package = with pkgs; [
        adwaita-qt
        adwaita-qt6
      ];
    };
    home.sessionVariables = {
      # This i think fixes some warns/slow launches on some qt apps
      QT_QPA_PLATFORM_PLUGIN_PATH = "${pkgs.libsForQt5.qt5.qtbase.bin}/lib/qt-${pkgs.libsForQt5.qt5.qtbase.version}/plugins/platforms";
      # Fake running KDE
      # https://wiki.archlinux.org/title/qt#Configuration_of_Qt_5_applications_under_environments_other_than_KDE_Plasma
      # https://wiki.archlinux.org/title/Uniform_look_for_Qt_and_GTK_applications#The_KDE_Plasma_XDG_Desktop_Portal_is_not_being_used
      DESKTOP_SESSION = "KDE";
    };
  };
}
