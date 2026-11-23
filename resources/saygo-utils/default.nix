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

  # shhell bakes in its GHC environment and GHCi.
  shhellEnv = drv:
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

  # Each executable's main-is, from saygo-utils.cabal.
  executables = pkgs.lib.pipe ./saygo-utils.cabal [
    builtins.readFile
    (pkgs.lib.splitString "\n")
    (pkgs.lib.foldl' (acc: line: let
      name = builtins.match "executable (.*)" line;
      main = builtins.match " *main-is: *(.*)" line;
    in
      if name != null
      then acc // {current = pkgs.lib.head name;}
      else if main != null && acc ? current
      then acc // {${acc.current} = pkgs.lib.head main;}
      else acc) {})
    (pkgs.lib.filterAttrs (name: _: name != "current"))
  ];

  # Each program is built on its own, from the cabal file, its own source and
  # src/Bin.hs, so changing one rebuilds only that one. shhell's helpers are
  # built with it, since it finds them next to itself.
  group = main:
    if builtins.elem main ["src/shhell.hs" "src/ghc-link-cache.hs" "src/shhell-bench.hs"]
    then "shhell"
    else main;
  # Each executable's derivation, by name; the ones built together share one.
  programs = pkgs.lib.concatMapAttrs (key: names: let
    mains = pkgs.lib.unique (map (name: executables.${name}) names);
    drv = mkSaygoUtils {
      src = pkgs.lib.fileset.toSource {
        root = ./.;
        fileset = pkgs.lib.fileset.unions (
          [./saygo-utils.cabal ./src/Bin.hs]
          ++ map (main: ./. + "/${main}") mains
          # The library, which only shhell-bench uses.
          ++ pkgs.lib.optional (key == "shhell") ./src/Shhell
        );
      };
      overrideCabal = {
        pname = "saygo-utils-${pkgs.lib.removeSuffix ".hs" (baseNameOf key)}";
        isLibrary = false;
        buildTarget = toString (map (name: "exe:${name}") names);
        env.RUNTIME_INPUTS = runtimeInputs;
      };
      developPackageArgs = pkgs.lib.optionalAttrs (builtins.elem "shhell" names) {modifier = shhellEnv;};
    };
  in
    pkgs.lib.genAttrs names (_: drv)) (pkgs.lib.groupBy (name: group executables.${name}) (builtins.attrNames executables));
in
  pkgs.symlinkJoin {
    name = "saygo-utils";
    paths = pkgs.lib.unique (builtins.attrValues programs);
    nativeBuildInputs = pkgs.lib.optional (lazyPackages != []) pkgs.makeWrapper;
    postBuild = pkgs.lib.optionalString (lazyPackages != []) ''
      wrapProgram $out/bin/comma-bg --set COMMA_BG_LAZY_PACKAGES \
        ${pkgs.writeText "lazy-packages.json" (builtins.toJSON (map lazy lazyPackages))}
    '';
    passthru = {
      # For apps in flake.nix, so running one program builds only that one.
      inherit programs;
      # For haskellShell in flake.nix.
      inherit runtimeInputs;
      inherit (mkSaygoUtils {}) env;
    };
  }
