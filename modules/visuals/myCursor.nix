{
  pkgs,
  conUsername,
  ...
}: let
  cursorName = "Capitaine Cursors (Gruvbox)";
  cursorPkg = pkgs.capitaine-cursors-themed;
  cursorSize = 32;
in {
  stylix.cursor.name = cursorName;
  stylix.cursor.package = cursorPkg;
  stylix.cursor.size = cursorSize;

  home-manager.users.${conUsername} = {
    home.pointerCursor = {
      enable = true;
      x11.enable = true;
      gtk.enable = true;
      name = cursorName;
      # package = cursorPkg; # Set by stylix
      size = cursorSize;
    };

    gtk = {
      enable = true;
      cursorTheme = {
        name = cursorName;
        package = cursorPkg;
      };
    };
  };
}
