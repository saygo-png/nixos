{
  lib,
  pkgs,
  conHome,
  conUsername,
  ...
}: {
  home-manager.users.${conUsername} = {config, ...}: {
    qt = {
      enable = true;
      platformTheme.name = "qtct";
      style.package = with pkgs; [adwaita-qt adwaita-qt6];
    };

    home.sessionVariables = {
      QT_QPA_PLATFORMTHEME = "qt5ct";
    };

    home.file = let
      # Qt config
      colorSchemeName = "gruvbox-medium-dark";
      mkScheme = colors: lib.concatStringsSep ", " (map (color: "#ff${color}") colors);
      colorScheme = lib.generators.toINI {} {
        ColorScheme = with config.lib.stylix.colors; {
          active_colors = mkScheme [
            base06 # Window text
            base00 # Button background
            base06 # Bright
            base05 # Less bright
            base01 # Dark
            base02 # Less dark
            base06 # Normal text
            base07 # Bright text
            base06 # Button text
            base01 # Normal background
            base02 # Window
            base00 # Shadow
            base02 # Highlight
            base05 # Highlighted text
            base0D # Link
            base0E # Visited link
            base00 # Alternate background
            base01 # Default
            base01 # Tooltip background
            base06 # Tooltip text
            base05 # Placeholder text
          ];

          inactive_colors = mkScheme [
            base04 # Window text
            base00 # Button background
            base05 # Bright
            base04 # Less bright
            base01 # Dark
            base02 # Less dark
            base04 # Normal text
            base05 # Bright text
            base04 # Button text
            base01 # Normal background
            base02 # Window
            base00 # Shadow
            base02 # Highlight
            base05 # Highlighted text
            base0D # Link
            base0E # Visited link
            base00 # Alternate background
            base01 # Default
            base01 # Tooltip background
            base05 # Tooltip text
            base04 # Placeholder text
          ];

          disabled_colors = mkScheme [
            base04 # Window text
            base00 # Button background
            base04 # Bright
            base03 # Less bright
            base00 # Dark
            base01 # Less dark
            base04 # Normal text
            base05 # Bright text
            base04 # Button text
            base01 # Normal background
            base02 # Window
            base00 # Shadow
            base02 # Highlight
            base05 # Highlighted text
            base0D # Link
            base0E # Visited link
            base00 # Alternate background
            base01 # Default
            base01 # Tooltip background
            base04 # Tooltip text
            base03 # Placeholder text
          ];
        };
      };

      baseConfig = {
        Appearance = {
          custom_palette = true;
          style = "Adwaita-Dark";
          standard_dialogs = "default";
          icon_theme = config.gtk.iconTheme.name;
          color_scheme_path = "${conHome}/.config/qt5ct/colors/${colorSchemeName}.conf";
        };

        Troubleshooting = {
          force_raster_widgets = 1;
          ignored_applications = "@Invalid()";
        };

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
      # QT theme
      ".config/qt5ct/colors/${colorSchemeName}.conf".text = colorScheme;
      ".config/qt6ct/colors/${colorSchemeName}.conf".text = colorScheme;
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
  };
}
