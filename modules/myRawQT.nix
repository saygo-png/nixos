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
      platformTheme.name = "qtct";
      style.package = with pkgs; [
        adwaita-qt
        adwaita-qt6
      ];
    };

    home.sessionVariables = {
      QT_QPA_PLATFORM_PLUGIN_PATH = "${pkgs.libsForQt5.qt5.qtbase.bin}/lib/qt-${pkgs.libsForQt5.qt5.qtbase.version}/plugins/platforms";
      # Fake running KDE
      # https://wiki.archlinux.org/title/qt#Configuration_of_Qt_5_applications_under_environments_other_than_KDE_Plasma
      # https://wiki.archlinux.org/title/Uniform_look_for_Qt_and_GTK_applications#The_KDE_Plasma_XDG_Desktop_Portal_is_not_being_used
      DESKTOP_SESSION = "KDE";
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
            base00 # Normal background
            base00 # Window
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
            base00 # Normal background
            base00 # Window
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
            base00 # Normal background
            base00 # Window
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
          color_scheme_path = "${conHome}/.config/qt5ct/colors/${colorSchemeName}.conf";
          custom_palette = true;
          icon_theme = config.gtk.iconTheme.name;
          standard_dialogs = "default";
          # style = "Fusion";
          style = "Adwaita-Dark";
        };

        Troubleshooting = {
          force_raster_widgets = 1;
          ignored_applications = "@Invalid()";
        };

        Interface = {
          cursor_flash_time = 1200;
          double_click_interval = 400;
          menus_have_icons = true;
          show_shortcuts_in_context_menus = true;
          gui_effects = "@Invalid()";
          stylesheets = "@Invalid()";
          buttonbox_layout = 3; # GNOME dialog button layout
          toolbutton_style = 4; # Follow the application style
          activate_item_on_single_click = 1; # ... - i think that means let the application decide
          dialog_buttons_have_icons = 1; # ...
          underline_shortcut = 1; # ...
          wheel_scroll_lines = 3;
          keyboard_scheme = 2; # X11
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
