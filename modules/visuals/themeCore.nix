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

    kdePackages.qtsvg # Icons for dolphin
    kdePackages.qtwayland # qt6
    libsForQt5.qt5.qtwayland

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
      pkgs.noto-fonts-cjk-sans
      pkgs.noto-fonts-cjk-serif
      pkgs.noto-fonts-color-emoji
      pkgs.nerd-fonts.symbols-only
    ];
    fontconfig.defaultFonts = lib.mapAttrs (_: v: [v] ++ ["${config.stylix.fonts.emoji.name}"]) {
      serif = config.stylix.fonts.serif.name;
      sansSerif = config.stylix.fonts.sansSerif.name;
      monospace = config.stylix.fonts.monospace.name;
    };
  };
  stylix = {
    enable = true;
    autoEnable = true;

    # This option is seemingly broken and makes a non fitting ugly theme
    targets.chromium.enable = false;
  };

  xdg.portal = {
    enable = true;
    xdgOpenUsePortal = false;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
  };

  # Fixes some themeing/cursor issues.
  programs.dconf.enable = lib.mkDefault true;

  home-manager.users.${conUsername} = {
    xdg.configFile."wallpaper.png".source = config.stylix.image;

    # This is used for Saywallpaper
    xdg.configFile."wallpaper.raw".source = let
      rawWallPkg =
        pkgs.runCommand "make raw wallpaper" {nativeBuildInputs = [pkgs.imagemagick];}
        ''
          magick "${config.stylix.image}" -gravity Center -crop 1920x1080+0+0 +repage wallpaper_cropped.png
          magick wallpaper_cropped.png -depth 8 bgra:wallpaper.raw
          mkdir $out
          mv wallpaper.raw $out
        '';
    in "${rawWallPkg}/wallpaper.raw";

    gtk.enable = true;
    qt.enable = lib.mkDefault true;
  };
}
