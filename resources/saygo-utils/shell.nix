{pkgs ? import <nixpkgs> {}}:
pkgs.mkShell {
  packages = let
    ghcPackages = pkgs.haskell.packages.ghc984;
  in [
    ghcPackages.haskell-language-server
    ghcPackages.ghc
  ];
}
