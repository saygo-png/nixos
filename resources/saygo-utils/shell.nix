{
  pkgs ?
    import (
      fetchTree {
        type = "github";
        owner = "NixOS";
        repo = "nixpkgs";
        rev = "f4b140d5b253f5e2a1ff4e5506edbf8267724bde";
      }
    ) {},
}:
pkgs.mkShell {
  packages = let
    ghcPackages = pkgs.haskell.packages.ghc984;
  in [
    ghcPackages.haskell-language-server
    ghcPackages.ghc
    ghcPackages.cabal-install
  ];
}
