{
  niceHaskell,
  pkgs,
  ...
}:
niceHaskell.mkPackage {
  flags = niceHaskell.mkFlags {doCheck = false;};
  packageRoot = ./.;
  cabalName = "saygo-utils";
  developPackageArgs = {
    modifier = drv:
      drv.overrideAttrs (_: let
        hp = pkgs.haskellPackages;
        ghcEnv = hp.ghcWithPackages (p: with p; [shh shh-extras relude]);
      in {SHHELL_GHC_ENV = ghcEnv;});
  };
}
