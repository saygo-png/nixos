{
  lib,
  pkgs,
  config,
  conUsername,
  ...
}: {
  environment.systemPackages = with pkgs; [
    # Gtk fixes
    gnome-themes-extra
    gtk-engine-murrine

    # Sass thing for themes
    sassc

    kdePackages.qtsvg # Icons for dolphin
    kdePackages.qtwayland # qt6
    libsForQt5.qt5.qtwayland

    # fix kirigami apps look
    # for example in filelight, without it the app looks weird
    # https://github.com/NixOS/nixpkgs/pull/202990#issuecomment-1328068486
    kdePackages.qqc2-desktop-style # qt6
    libsForQt5.qqc2-desktop-style

    # Some KDE applications such as Dolphin try to fall back to Breeze
    # theme icons. Lets make sure they're also found.
    kdePackages.breeze-icons
    qt6.qtsvg # needed to load breeze icons

    # Needed for some icon packs
    adwaita-icon-theme
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
      pkgs.nerd-fonts.symbols-only
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
  xdg.portal.extraPortals = [pkgs.xdg-desktop-portal-gtk];

  stylix.targets.chromium.enable = false; # This option is seemingly broken and makes a non fitting ugly theme

  # Fixes some themeing/cursor issues.
  programs.dconf.enable = lib.mkDefault true;

  home-manager.users.${conUsername} = {
    home.sessionVariables = {
      # This i think fixes some warns/slow launches on some qt apps
      QT_QPA_PLATFORM_PLUGIN_PATH = "${pkgs.libsForQt5.qt5.qtbase.bin}/lib/qt-${pkgs.libsForQt5.qt5.qtbase.version}/plugins/platforms";
    };

    xdg.configFile."wallpaper.png".source = config.stylix.image;

    gtk = {
      enable = true;
    };
    qt = lib.mkDefault {
      enable = true;
    };
  };
}
