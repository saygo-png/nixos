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
        style = "full";
        autohint = false;
      };
      subpixel = {
        rgba = "none";
        lcdfilter = "default";
      };
      localConf = ''
        <?xml version="1.0"?>
        <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
        <fontconfig>
          <match target="font">
            <test name="family" compare="contains">
              <string>Courier Prime</string>
            </test>
            <edit name="hinting" mode="assign">
              <bool>true</bool>
            </edit>
            <edit name="autohint" mode="assign">
              <bool>true</bool>
            </edit>
            <edit name="hintstyle" mode="assign">
              <const>hintfull</const>
            </edit>
            <edit name="antialias" mode="assign">
              <bool>true</bool>
            </edit>
          </match>
        </fontconfig>
      '';
    };
  }; # }}}

  # Stylix {{{
  stylix = {
    polarity = "dark";
    image = "${inputs.extras-nixos}/wallpaper.png";
    base16Scheme = {
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
  };

  stylix.fonts = let
    courier-patched = pkgs.courier-prime.overrideAttrs {
      src = "${inputs.extras-nixos}/courier-prime-no-ligatures";
      installPhase = ''
        runHook preInstall

        install -m444 -Dt $out/share/fonts/truetype *.ttf

        runHook postInstall
      '';
    };
    courierSet = {
      name = "Courier Prime";
      package = courier-patched;
    };
  in {
    monospace = courierSet;
    sansSerif = courierSet;
    serif = courierSet;
    emoji = {
      name = "Symbols Nerd Font";
      package = pkgs.nerd-fonts.symbols-only;
    };

    sizes = {
      popups = lib.mkDefault 12;
      desktop = lib.mkDefault 12;
      applications = lib.mkDefault 12;
      terminal = 13;
    };
  };

  stylix.opacity = {
    popups = 0.7;
    desktop = 0.7;
    terminal = 0.7;
    applications = 0.7;
  };

  home-manager.users.${conUsername} = {
    stylix.iconTheme = {
      enable = true;
      package = pkgs.gruvbox-plus-icons;
      dark = "Gruvbox-Plus-Dark";
      light = "Gruvbox-Plus-Light";
    };

    dconf.settings = {
      # Remove min and max buttons
      "org/gnome/desktop/wm/preferences".button-layout = ":appmenu";
      # Prefer darkmode
      "org/gnome/desktop/interface".color-scheme = "prefer-dark";
    };
  };
}
