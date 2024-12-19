{
  pkgs,
  conUsername,
  ...
}: {
  home-manager.users.${conUsername} = {
    qt = {
      enable = true;
      platformTheme.name = "adwaita";
      style.name = "adwaita-dark";
      style.package = with pkgs; [adwaita-qt adwaita-qt6];
    };
  };
}
