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

  # The packages in each executable's x-runtime-inputs in saygo-utils.cabal, as
  # "<package> <path>" lines for src/Bin.hs.
  runtimeInputs = pkgs.lib.pipe ./saygo-utils.cabal [
    builtins.readFile
    (pkgs.lib.splitString "\n")
    (map (builtins.match " *x-runtime-inputs:(.*)"))
    (pkgs.lib.concatMap (match: pkgs.lib.optionals (match != null) (pkgs.lib.splitString "," (pkgs.lib.head match))))
    (map pkgs.lib.trim)
    pkgs.lib.unique
    (map (name: "${name} ${pkgs.lib.getBin pkgs.${name}}"))
    pkgs.lib.concatLines
  ];

  mkSaygoUtils = args:
    niceHaskell.mkPackageNoIFD ({
        inherit haskellPackages;
        src = ./.;
        cabal2nix = ./cabal.nix;
      }
      // args);
  # Only the library, for shhell's GHC environment. The whole package can't be
  # in it, since shhell is built against that environment.
  library = mkSaygoUtils {
    src = pkgs.lib.fileset.toSource {
      root = ./.;
      fileset = pkgs.lib.fileset.unions [./saygo-utils.cabal ./src/Shhell];
    };
    overrideCabal.buildTarget = "lib:saygo-utils";
  };

  saygo-utils = mkSaygoUtils {
    overrideCabal = {
      env.RUNTIME_INPUTS = runtimeInputs;
      # For haskellShell in flake.nix.
      passthru = {inherit runtimeInputs;};
    };
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
