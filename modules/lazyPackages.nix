# Packages that comma-bg evaluates and builds only when launched.
{
  lib,
  pkgs,
  config,
  ...
}: let
  # Runs a command and, if it fails, keeps the window open to read the error.
  loading = pkgs.writeShellApplication {
    name = "lazy-loading";
    text = ''"$@" || read -rp "Failed. Press Enter to close. "'';
  };
in {
  options.custom.lazyPackages = lib.mkOption {
    type = lib.types.listOf lib.types.package;
    default = [];
  };

  config = {
    custom.lazyPackages = [
      pkgs.blender
      pkgs.audacity
      pkgs.pitivi
      pkgs.localsend
      pkgs.inkscape
    ];

    # A desktop entry for a lazy package. Its command runs in a terminal, so nix
    # fetching the package is visible, and the window closes once it is done.
    _module.args.mkLazyDesktopItem = {
      exec,
      desktopName,
      ...
    } @ args:
      pkgs.makeDesktopItem (args
        // {
          exec = "${lib.getExe config.custom.defaultTerminal.package} --title \"Loading ${desktopName}\" -e ${lib.getExe loading} ${exec}";
        });
  };
}
