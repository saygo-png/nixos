{
  niceHaskell,
  pkgs,
  shhellHaskellPackages ? pkgs.haskellPackages,
  ...
}: let
  mkSaygoUtils = args:
    niceHaskell.mkPackage ({
        flags = niceHaskell.mkFlags {doCheck = false;};
        packageRoot = ./.;
        cabalName = "saygo-utils";
      }
      // args);
  # Only the library, for shhell's GHC environment. The whole package can't be
  # in it, since shhell is built against that environment. It's built from the
  # environment's package set, which can be another nixpkgs than niceHaskell's.
  library = mkSaygoUtils {
    haskellPackages = shhellHaskellPackages;
    overrideCabalOverride = _: {buildTarget = "lib:saygo-utils";};
  };
in
  mkSaygoUtils {
    developPackageArgs = {
      modifier = drv:
        drv.overrideAttrs (_: let
          hp = shhellHaskellPackages;
          ghcEnv = hp.ghcWithPackages (p: with p; [shh shh-extras relude template-haskell Glob PyF library]);
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
