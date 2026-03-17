{
  inputs,
  conUsername,
  system,
  pkgs,
  ...
}: let
  hyprqt6engine = inputs.hyprqt6engine.packages.${system}.hyprqt6engine;
in {
  stylix.targets.qt.enable = false;
  environment.systemPackages = [
    pkgs.carla
    pkgs.kdePackages.breeze
    pkgs.kdePackages.breeze-icons
    pkgs.kdePackages.qqc2-breeze-style
    pkgs.kdePackages.kcolorscheme
    hyprqt6engine

    pkgs.hyprland-qt-support
    pkgs.hyprland-qtutils
  ];

  environment.variables = {
    QT_QPA_PLATFORMTHEME = "hyprqt6engine";
    QT_PLUGIN_PATH = "${pkgs.qt6.qtbase}/${pkgs.qt6.qtbase.qtPluginPrefix}:${hyprqt6engine}/lib/qt-6";
  };
  home-manager.users.${conUsername} = { ...}: {
    stylix.targets.qt.enable = false;
    stylix.targets.kde.enable = false;
    qt = {
      enable = true;
      platformTheme.name = "hyprqt6engine";
      style = {
        name = "Breeze";
        package = pkgs.kdePackages.breeze;
      };
    };

    home = {
      file = {
        ".config/hypr/hyprqt6engine.conf".text = ''
          theme {
            color_scheme = ${pkgs.kdePackages.breeze}/share/color-schemes/BreezeLight.colors
            style = Breeze
            icon_theme = breeze-light
            font_size = 13
            font_fixed_size = 13
          }
        '';
      };
    };
  };
}
