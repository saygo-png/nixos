{
  lib,
  pkgs,
  conUsername,
  ...
}: {
  # Fonts. {{{
  fonts = {
    fontconfig = {
      enable = true;
      antialias = true;
      hinting = {
        enable = true;
        style = "full";
        autohint = false;
      };
      subpixel = {
        rgba = "none";
        lcdfilter = "default";
      };
    };
  }; # }}}

  # Stylix {{{
  stylix.cursor.size = 32;
  stylix.polarity = "dark";
  stylix.cursor.name = "Capitaine Cursors (Gruvbox)";
  stylix.cursor.package = pkgs.capitaine-cursors-themed;
  stylix.image = ../../resources/wallpaper.png;
  stylix.base16Scheme = {
    base00 = "282828"; # #282828 dark  ----
    base01 = "3c3836"; # #3c3836 dark  ---
    base02 = "504945"; # #504945 dark  --
    base03 = "665c54"; # #665c54 dark  -
    base04 = "bdae93"; # #bdae93 light +
    base05 = "d5c4a1"; # #d5c4a1 light ++
    base06 = "ebdbb2"; # #ebdbb2 light +++
    base07 = "fbf1c7"; # #fbf1c7 light ++++
    base08 = "fb4934"; # #fb4934 red
    base09 = "fe8019"; # #fe8019 orange
    base0A = "fabd2f"; # #fabd2f yellow
    base0B = "b8bb26"; # #b8bb26 green
    base0C = "8ec07c"; # #8ec07c cyan
    base0D = "83a598"; # #83a598 blue
    base0E = "d3869b"; # #d3869b purple
    base0F = "d65d0e"; # #d65d0e brown
  };

  stylix.fonts = {
    monospace = {
      name = "Courier Prime";
      package = pkgs.courier-prime;
    };
    sansSerif = {
      name = "Courier Prime";
      package = pkgs.courier-prime;
    };
    serif = {
      name = "Courier Prime";
      package = pkgs.courier-prime;
    };
    emoji = {
      name = "Symbols Nerd Font";
      package = pkgs.nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];};
    };
  };

  stylix.fonts.sizes = {
    popups = lib.mkDefault 12;
    desktop = lib.mkDefault 12;
    terminal = lib.mkDefault 13;
    applications = lib.mkDefault 11;
  };

  stylix.opacity = {
    popups = 0.5;
    desktop = 0.5;
    terminal = 0.5;
    applications = 0.5;
  };

  # stylix.opacity = {
  #   popups = 1.0;
  #   desktop = 1.0;
  #   terminal = 1.0;
  #   applications = 1.0;
  # }; # }}}

  home-manager.users.${conUsername} = {
    dconf.settings = {
      # Remove min and max buttons
      "org/gnome/desktop/wm/preferences".button-layout = ":appmenu";
      # Prefer darkmode
      "org/gnome/desktop/interface".color-scheme = "prefer-dark";
    };

    gtk = {
      gtk3.extraConfig.gtk-application-prefer-dark-theme = 1;
      gtk4.extraConfig.gtk-application-prefer-dark-theme = 1;
      iconTheme = {
        name = "Gruvbox-Plus-Dark";
        package = pkgs.gruvbox-plus-icons;
      };
    };
  };
}
