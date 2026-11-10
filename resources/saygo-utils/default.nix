{
  niceHaskell,
  pkgs,
  haskellPackages ? pkgs.haskellPackages,
  # Given to comma-bg as JSON, with the installable it evaluates for each one.
  lazyPackages ? [],
  lazyInstallable ? null,
  ...
}: let
  lazy = pkg: let
    name = pkgs.lib.getName pkg;
  in {
    inherit name;
    installable = "${lazyInstallable name}^${(pkgs.lib.getBin pkg).outputName}";
    program = pkg.meta.mainProgram or name;
  };

  mkSaygoUtils = args:
    niceHaskell.mkPackageNoIFD ({
        inherit haskellPackages;
        src = ./.;
        cabal2nix = ./cabal.nix;
      }
      // args);
  # Only the library, for shhell's GHC environment. The whole package can't be
  # in it, since shhell is built against that environment.
  library = mkSaygoUtils {overrideCabal.buildTarget = "lib:saygo-utils";};

  saygo-utils = mkSaygoUtils {
    developPackageArgs = {
      modifier = drv:
        drv.overrideAttrs (_: let
          hp = haskellPackages;
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
  };
in
  if lazyPackages == []
  then saygo-utils
  else
    pkgs.symlinkJoin {
      inherit (saygo-utils) name;
      paths = [saygo-utils];
      nativeBuildInputs = [pkgs.makeWrapper];
      postBuild = ''
        wrapProgram $out/bin/comma-bg --set COMMA_BG_LAZY_PACKAGES \
          ${pkgs.writeText "lazy-packages.json" (builtins.toJSON (map lazy lazyPackages))}
      '';
    }
