{
  lib,
  pkgs,
  config,
  conUsername,
  ...
}: {
  environment.systemPackages = with pkgs; [
    kdePackages.qtsvg # Icons for dolphin
    kdePackages.qtwayland # qt6
    libsForQt5.qt5.qtwayland
    gtk-engine-murrine

    # fix kirigami apps look
    # for example in filelight, without it the app looks weird
    # https://github.com/NixOS/nixpkgs/pull/202990#issuecomment-1328068486
    kdePackages.qqc2-desktop-style # qt6
    libsForQt5.qqc2-desktop-style

    # Some KDE applications such as Dolphin try to fall back to Breeze
    # theme icons. Lets make sure they're also found.
    libsForQt5.breeze-qt5
    kdePackages.breeze-icons
    qt6.qtsvg # needed to load breeze icons
  ];

  # Fonts. {{{
  fonts = {
    packages = [
      # Main fonts.
      config.stylix.fonts.serif.package
      config.stylix.fonts.monospace.package
      config.stylix.fonts.sansSerif.package

      pkgs.roboto
      pkgs.noto-fonts
      pkgs.jetbrains-mono
      pkgs.noto-fonts-emoji
      pkgs.noto-fonts-cjk-sans
      pkgs.noto-fonts-cjk-serif
      (pkgs.nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
    ];
    fontconfig = {
      defaultFonts = {
        serif = ["${config.stylix.fonts.serif.name}" "${config.stylix.fonts.emoji.name}"];
        sansSerif = ["${config.stylix.fonts.sansSerif.name}" "${config.stylix.fonts.emoji.name}"];
        monospace = ["${config.stylix.fonts.monospace.name}" "${config.stylix.fonts.emoji.name}"];
      };
    };
  }; # }}}

  # Stylix {{{
  stylix.enable = true;
  stylix.autoEnable = true;

  xdg.portal.enable = true;
  xdg.portal.xdgOpenUsePortal = false;
  xdg.portal.config.common.default = "*";
  xdg.portal.extraPortals = [pkgs.xdg-desktop-portal-gtk];

  # Fixes some themeing/cursor issues.
  programs.dconf.enable = lib.mkDefault true;

  home-manager.users.${conUsername} = {
    home.pointerCursor = {
      x11.enable = lib.mkForce true;
      gtk.enable = lib.mkForce true;
    };

    home.sessionVariables = {
      # This i think fixes some warns/slow launches on some qt apps
      QT_QPA_PLATFORM_PLUGIN_PATH = "${pkgs.libsForQt5.qt5.qtbase.bin}/lib/qt-${pkgs.libsForQt5.qt5.qtbase.version}/plugins/platforms";
      # Fake running KDE
      # https://wiki.archlinux.org/title/qt#Configuration_of_Qt_5_applications_under_environments_other_than_KDE_Plasma
      # https://wiki.archlinux.org/title/Uniform_look_for_Qt_and_GTK_applications#The_KDE_Plasma_XDG_Desktop_Portal_is_not_being_used
      DESKTOP_SESSION = "KDE";
      CALIBRE_USE_DARK_PALETTE = "1";
    };

    xdg.configFile."wallpaper.png".source = config.stylix.image;

    # Hopefully will set dark mode properly
    stylix.targets.gnome.enable = true;
    stylix.targets.kde.enable = true;

    gtk = {
      enable = true;
    };
    qt = lib.mkDefault {
      enable = true;
    };
  };
}
