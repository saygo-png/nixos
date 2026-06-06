{
  conUsername,
  pkgs,
  lib,
  ...
}: {
  stylix.targets.qt.enable = false;

  nixpkgs.overlays = [
    (_: prev: {
      qt6ct = prev.qt6ct.overrideAttrs (oldAttrs: {
        patches = (oldAttrs.patches or []) ++ [(lib.my.relativeToRoot "resources/qt6ct-0.11.patch")];
        name = "qt6ct-kde";
      });
    })
  ];

  environment.systemPackages = [
    pkgs.kdePackages.breeze
    pkgs.kdePackages.breeze-icons
    pkgs.kdePackages.qqc2-breeze-style
    pkgs.kdePackages.kcolorscheme
    pkgs.kdePackages.qt6ct
    pkgs.kdePackages.plasma-integration
    pkgs.kdePackages.qqc2-desktop-style
    pkgs.kdePackages.kirigami
  ];

  qt.enable = true;
  home-manager.users.${conUsername} = {...}: {
    stylix.targets.qt.enable = false;
    stylix.targets.kde.enable = false;
    qt = {
      enable = true;
      platformTheme.name = "qtct";
      qt6ctSettings = {
        Appearance = {
          style = "Breeze";
          standard_dialogs = "xdgdesktopportal";
        };
        Fonts = {
          fixed = ''"Courier Prime,12"'';
          general = ''"Courier Prime,12"'';
        };
      };
    };
    xdg.configFile = {
      "kdeglobals".text = ''
        [UiSettings]
        ColorScheme=BreezeDark
      '';
      "dolphinrc".text = ''
        [UiSettings]
        ColorScheme=BreezeDark
      '';
    };
  };
}
