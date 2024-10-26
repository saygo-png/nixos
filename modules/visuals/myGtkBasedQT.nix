{
  lib,
  pkgs,
  conUsername,
  ...
}: {
  home-manager.users.${conUsername} = {
    gtk = {
      enable = true;
      theme = {
        package = lib.mkForce pkgs.gruvbox-dark-gtk;
        name = lib.mkForce "gruvbox-dark";
      };
      iconTheme = {
        package = pkgs.papirus-icon-theme;
        name = "Papirus-Dark";
      };
      font = {
        name = lib.mkForce "Courier Prime";
        package = lib.mkForce pkgs.courier-prime;
      };
      cursorTheme = {
        package = lib.mkForce pkgs.capitaine-cursors-themed;
        name = lib.mkForce "Capitaine Cursors (Gruvbox)";
      };
    };
    qt = {
      enable = true;
      platformTheme.name = "gtk";
    };
  };
}
