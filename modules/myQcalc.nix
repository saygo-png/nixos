{
  lib,
  pkgs,
  conUsername,
  ...
}: {
  environment.systemPackages = [
    pkgs.qalculate-gtk # Gui calculator
    pkgs.libqalculate # Calculator
  ];

  home-manager.users.${conUsername} = _: {
    xdg.configFile."qalculate/qalc.cfg" = {
      text = lib.generators.toINI {} {
        Mode = {
          calculate_as_you_type = 1;
        };
      };
    };

    home.shellAliases = {
      "qcalc" = "qalc";
    };
  };
}
