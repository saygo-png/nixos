{
  niceHaskell,
  pkgs,
  shhellHaskellPackages ? pkgs.haskellPackages,
  ...
}:
niceHaskell.mkPackage {
  flags = niceHaskell.mkFlags {doCheck = false;};
  packageRoot = ./.;
  cabalName = "saygo-utils";
  developPackageArgs = {
    modifier = drv:
      drv.overrideAttrs (_: let
        hp = shhellHaskellPackages;
        ghcEnv = hp.ghcWithPackages (p: with p; [shh shh-extras relude template-haskell Glob PyF]);
        ghci = pkgs.callPackage ./ghci.nix {
          inherit hp;
          env = ghcEnv;
        };
      in {
        SHHELL_GHC_ENV = ghcEnv;
        SHHELL_GHCI = "${ghci}/bin/ghci";
      });
  };
}
