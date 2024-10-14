{
  lib,
  pkgs,
  config,
  conUsername,
  conFlakePathRel,
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

  # Fonts. {{{
  fonts = {
    packages = with pkgs; [
      # Main font.
      courier-prime

      roboto
      noto-fonts
      jetbrains-mono
      noto-fonts-emoji
      noto-fonts-cjk-sans
      noto-fonts-cjk-serif
      (nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
    ];

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

      defaultFonts = {
        serif = ["Courier Prime" "Symbols Nerd Font"];
        sansSerif = ["Courier Prime" "Symbols Nerd Font"];
        monospace = ["Courier Prime" "Symbols Nerd Font"];
      };
    };
  }; # }}}

  # Stylix {{{
  stylix.enable = true;
  stylix.cursor.size = 32;
  stylix.autoEnable = true;
  stylix.polarity = "dark";
  stylix.targets.grub.useImage = true;
  stylix.cursor.name = "Capitaine Cursors (Gruvbox)";
  stylix.cursor.package = pkgs.capitaine-cursors-themed;
  stylix.image = ../resources/wallpaper.png;
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

  # stylix.opacity = {
  #   popups = 0.7;
  #   desktop = 0.7;
  #   terminal = 0.7;
  #   applications = 0.5;
  # };

  stylix.opacity = {
    popups = 1.0;
    desktop = 1.0;
    terminal = 1.0;
    applications = 1.0;
  }; # }}}

  xdg.portal.enable = true;
  xdg.portal.xdgOpenUsePortal = false;
  xdg.portal.config.common.default = "*";
  xdg.portal.extraPortals = [pkgs.xdg-desktop-portal-gtk];

  # Fixes some themeing/cursor issues.
  programs.dconf.enable = lib.mkDefault true;

  home-manager.users.${conUsername} = {
    xdg.configFile."wallpaper.png".source = config.stylix.image;
    # Needed for transparency.
    stylix.targets.fzf.enable = false;

    # Hopefully will set dark mode properly
    stylix.targets.gnome.enable = true;
    stylix.targets.kde.enable = true;

    dconf.settings = {
      # Remove min and max buttons
      "org/gnome/desktop/wm/preferences".button-layout = ":appmenu";
      # Prefer darkmode
      "org/gnome/desktop/interface".color-scheme = "prefer-dark";
    };
    gtk = {
      enable = true;
      theme = {
        package = lib.mkForce pkgs.gruvbox-dark-gtk;
        name = lib.mkForce "gruvbox-dark";
      };
      # iconTheme = {
      #   name = "Gruvbox-Plus-Dark";
      #   package = pkgs.gruvbox-plus-icons;
      # };
      iconTheme = {
        package = pkgs.papirus-icon-theme;
        name = "Papirus-Dark";
      };
      font = {
        name = lib.mkForce "Courier Prime";
        package = lib.mkForce pkgs.courier-prime;
      };
      cursorTheme = {
        package = lib.mkForce pkgs.capitaine-cursors-themed;
        name = lib.mkForce "Capitaine Cursors (Gruvbox)";
      };
      gtk3.extraConfig.gtk-application-prefer-dark-theme = 1;
      gtk4.extraConfig.gtk-application-prefer-dark-theme = 1;
    };
    qt = {
      enable = true;
      platformTheme.name = "gtk";
    };
    home.pointerCursor = {
      x11.enable = lib.mkForce true;
      gtk.enable = true;
      package = lib.mkForce pkgs.capitaine-cursors-themed;
      name = lib.mkForce "Capitaine Cursors (Gruvbox)";
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
