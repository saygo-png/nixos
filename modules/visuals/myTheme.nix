{
  lib,
  pkgs,
  inputs,
  conUsername,
  ...
}: {
  imports = [
    ({config, ...}: {
      options = {
        const = config.constLib.mkConstsFromSet {
          accentColor = "7d8618";
        };
      };
    })
  ];
  # Fonts. {{{
  fonts = {
    fontconfig = {
      enable = true;
      antialias = true;
      hinting = {
        enable = true;
        style = "slight";
        autohint = true;
      };
      subpixel = {
        rgba = "none";
        lcdfilter = "default";
      };
    };
  }; # }}}

  # Stylix {{{
  stylix.polarity = "dark";
  stylix.image = "${inputs.extras-nixos}/wallpaper.png";
  stylix.base16Scheme = {
    base00 = "282828"; # #282828 dark  ----
    base01 = "3c3836"; # #3c3836 dark  ---
    base02 = "504945"; # #504945 dark  --
    base03 = "665c54"; # #665c54 dark  -
    base04 = "bdae93"; # #bdae93 light +
    base05 = "d5c4a1"; # #d5c4a1 light ++ # Default text
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
      package = pkgs.nerd-fonts.symbols-only;
    };
  };

  stylix.fonts.sizes = {
    popups = lib.mkDefault 12;
    desktop = lib.mkDefault 12;
    terminal = lib.mkDefault 12;
    applications = lib.mkDefault 12;
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
    stylix.iconTheme.enable = true;
    stylix.iconTheme.package = pkgs.gruvbox-plus-icons;
    stylix.iconTheme.dark = "Gruvbox-Plus-Dark";
    stylix.iconTheme.light = "Gruvbox-Plus-Light";

    dconf.settings = {
      # Remove min and max buttons
      "org/gnome/desktop/wm/preferences".button-layout = ":appmenu";
      # Prefer darkmode
      "org/gnome/desktop/interface".color-scheme = "prefer-dark";
    };

    gtk = {
      gtk3.extraConfig.gtk-application-prefer-dark-theme = 1;
      gtk4.extraConfig.gtk-application-prefer-dark-theme = 1;
    };
  };
}
