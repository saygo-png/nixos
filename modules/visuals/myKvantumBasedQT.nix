{
  lib,
  pkgs,
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

    xdg.configFile = {
      "Kvantum/kvantum.kvconfig".text = ''
        [General]
        theme=gruvbox-fallnn
      '';
      # "Kvantum/kvantum.kvconfig".text = ''
      #   [General]
      #   theme=KvGnomeDark#
      # '';

      "Kvantum/gruvbox-fallnn".source = lib.my.getSafePath "${inputs.gruvbox-kvantum}/gruvbox-kvantum";
      "Kvantum/KvGnomeDark#".source = lib.my.getSafePath "${pkgs.kdePackages.qtstyleplugin-kvantum}/share/Kvantum/KvGnomeDark/KvGnomeDark.kvconfig";
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
