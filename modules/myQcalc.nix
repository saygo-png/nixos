{
  lib,
  pkgs,
  conUsername,
  ...
}: {
  environment.systemPackages = [
    pkgs.qalculate-gtk # GUI frontend
    pkgs.libqalculate
  ];

  home-manager.users.${conUsername} = _: {
    home.shellAliases."qcalc" = "qalc";
    xdg.configFile."qalculate/qalc.cfg" = {
      text = lib.generators.toINI {} {
        Mode.calculate_as_you_type = 1;
      };
    };
  };
}
