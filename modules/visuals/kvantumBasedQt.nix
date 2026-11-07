{
  conUsername,
  pkgs,
  lib,
  ...
}: {
  stylix.targets.qt.enable = false;

  # https://github.com/nixos/nixpkgs/issues/489021
  nixpkgs.overlays = [
    (_: prev: {
      qt6Packages = prev.qt6Packages.overrideScope (_: q: {
        qt6ct = q.qt6ct.overrideAttrs (
          o: {
            patches = (o.patches or []) ++ [(lib.my.relativeToRoot "resources/qt6ct-0.11.patch")];
            # The patch only builds its KDE parts when these are found:
            buildInputs =
              o.buildInputs
              ++ [
                prev.kdePackages.kconfig
                prev.kdePackages.kcolorscheme
                prev.kdePackages.kiconthemes
              ];
          }
        );
      });
    })
  ];

  environment.systemPackages = [
    # The Breeze widget style.
    pkgs.kdePackages.breeze
    # The patched qt6ct sets this QtQuick style for every app, and Qt fails to
    # load QtQuick controls if it is missing. It imports Kirigami.
    pkgs.kdePackages.qqc2-desktop-style
    pkgs.kdePackages.kirigami
  ];

  qt.enable = true;
  home-manager.users.${conUsername} = {config, ...}: let
    # The same scheme Plasma applies by default, and the icons the GTK apps use.
    colorScheme = "${pkgs.kdePackages.breeze}/share/color-schemes/BreezeDark.colors";
    iconTheme = config.stylix.icons.dark;
  in {
    stylix.targets.qt.enable = false;
    stylix.targets.kde.enable = false;
    qt = {
      enable = true;
      platformTheme.name = "qtct";
      qt6ctSettings = {
        Appearance = {
          style = "Breeze";
          standard_dialogs = "xdgdesktopportal";
          custom_palette = true;
          color_scheme_path = colorScheme;
          icon_theme = iconTheme;
        };
        Fonts = {
          fixed = ''"Courier Prime,12"'';
          general = ''"Courier Prime,12"'';
        };
      };
    };
    xdg.configFile."kdeglobals".source = pkgs.runCommand "kdeglobals" {} ''
      cat ${colorScheme} > $out
      printf '\n[Icons]\nTheme=%s\n' ${lib.escapeShellArg iconTheme} >> $out
    '';
  };
}
