{
  lib,
  pkgs,
  conUsername,
  ...
}: {
  stylix.targets.qt.enable = false;
  home-manager.users.${conUsername} = {
    stylix.targets.qt.enable = false;
    qt = lib.mkDefault {
      style.name = "adwaita-dark";
      style.package = with pkgs; [adwaita-qt adwaita-qt6];
    };
  };
}
