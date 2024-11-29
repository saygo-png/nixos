{
  lib,
  pkgs,
  conUsername,
  ...
}: {
  home-manager.users.${conUsername} = {
    qt = lib.mkDefault {
      style.name = "adwaita-dark";
      style.package = with pkgs; [adwaita-qt adwaita-qt6];
    };
  };
}
