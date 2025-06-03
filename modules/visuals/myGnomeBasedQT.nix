{
  pkgs,
  conUsername,
  ...
}: {
  stylix.targets.qt.enable = false;
  home-manager.users.${conUsername} = {
    stylix.targets.qt.enable = false;
    qt = {
      enable = true;
      platformTheme.name = "adwaita";
      style.name = "adwaita-dark";
      style.package = with pkgs; [adwaita-qt adwaita-qt6];
    };
  };
}
