{
  lib,
  inputs,
  conUsername,
  ...
}: {
  stylix.targets.qt.enable = false;
  home-manager.users.${conUsername} = {config, ...}: {
    stylix.targets.qt.enable = false;

    qt = {
      enable = true;
      style.name = "kvantum";
      platformTheme.name = "qtct";
    };

    xdg.configFile = let
      theme = "Gruvbox-Dark-Green";
    in {
      "Kvantum/kvantum.kvconfig".text = ''
        [General]
        theme=${theme}
      '';

      "Kvantum/${theme}".source = lib.my.getSafePath "${inputs.gruvbox-kvantum}/${theme}";
    };
    home.file = let
      baseConfig = {
        Appearance = {
          custom_palette = false;
          style = "kvantum-Dark";
          icon_theme = config.gtk.iconTheme.name;
        };
      };
    in {
      ".config/qt5ct/qt5ct.conf".text = lib.generators.toINI {} (baseConfig
        // {
          Fonts.fixed = "\"${config.stylix.fonts.monospace.name},${toString config.stylix.fonts.sizes.applications},-1,5,50,0,0,0,0,0,Regular\"";
          Fonts.general = "\"${config.stylix.fonts.sansSerif.name},${toString config.stylix.fonts.sizes.applications},-1,5,50,0,0,0,0,0,Regular\"";
        });
      ".config/qt6ct/qt6ct.conf".text = lib.generators.toINI {} (baseConfig
        // {
          Fonts.fixed = "\"${config.stylix.fonts.monospace.name},${toString config.stylix.fonts.sizes.applications},-1,5,400,0,0,0,0,0,0,0,0,0,0,1,Regular\"";
          Fonts.general = "\"${config.stylix.fonts.sansSerif.name},${toString config.stylix.fonts.sizes.applications},-1,5,400,0,0,0,0,0,0,0,0,0,0,1,Regular\"";
        });
    };
    home.sessionVariables = {
      QT_QPA_PLATFORMTHEME = "qt5ct";
    };
  };
}
