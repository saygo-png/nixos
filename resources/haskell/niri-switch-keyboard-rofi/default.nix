{pkgs}: let
  hl = pkgs.haskell.lib;
in
  pkgs.haskellPackages.developPackage {
    root = ./.;
    returnShellEnv = false;
    modifier = drv:
      hl.dontHaddock (pkgs.haskell.lib.addBuildTools drv [
        pkgs.rofi-wayland
        pkgs.niri
      ]);
  }
