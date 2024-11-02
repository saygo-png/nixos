{
  conUsername,
  conFlakePathRel,
  lib,
  ...
}: {
  home-manager.users.${conUsername} = {config, ...}: {
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

      "Kvantum/gruvbox-fallnn".source = "${conFlakePathRel}/resources/qt/gruvbox-fallnn";
    };
    home.file = let
      baseConfig = {
        Appearance = {
          custom_palette = false;
          style = "kvantum-Dark";
          standard_dialogs = "default";
          icon_theme = config.gtk.iconTheme.name;
        };
        #
        # Troubleshooting = {
        #   force_raster_widgets = 1;
        #   ignored_applications = "@Invalid()";
        # };

        Interface = {
          wheel_scroll_lines = 3;
          menus_have_icons = true;
          cursor_flash_time = 1200;
          keyboard_scheme = 2; # X11
          gui_effects = "@Invalid()";
          stylesheets = "@Invalid()";
          double_click_interval = 400;
          underline_shortcut = 1; # ...
          dialog_buttons_have_icons = 1; # ...
          show_shortcuts_in_context_menus = true;
          buttonbox_layout = 3; # GNOME dialog button layout
          toolbutton_style = 4; # Follow the application style
          activate_item_on_single_click = 1; # ... - i think that means let the application decide
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
