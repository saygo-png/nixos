{
  niceHaskell,
  pkgs,
}: let
  inherit (pkgs) lib;

  fields = regex: lib.concatLists (lib.remove null (map (builtins.match regex) (lib.splitString "\n" (builtins.readFile ./saygo-utils.cabal))));

  executables = lib.listToAttrs (lib.zipListsWith lib.nameValuePair (fields "executable (.*)") (fields " *main-is: *(.*)"));

  source = files:
    lib.fileset.toSource {
      root = ./.;
      fileset = lib.fileset.unions ([./saygo-utils.cabal] ++ files);
    };

  mkPackage = haskellPackages: args:
    niceHaskell.mkPackageNoIFD ({
        inherit haskellPackages;
        src = ./.;
        cabal2nix = ./cabal.nix;
      }
      // args);
in {
  inherit mkPackage;

  runtimeInputs =
    lib.concatMapStrings (name: "${name} ${lib.getBin pkgs.${name}}\n")
    (lib.unique (map lib.trim (lib.concatMap (lib.splitString ",") (fields " *x-runtime-inputs:(.*)"))));

  withProcess = src: haskellPackages:
    haskellPackages.extend (_: super: {
      shh = super.shh.override {
        process =
          pkgs.haskell.lib.compose.overrideCabal (drv: {
            inherit src;
            libraryToolDepends = drv.libraryToolDepends or [] ++ [pkgs.autoreconfHook];
          })
          super.process_1_6_28_0;
      };
      shh-extras = pkgs.haskell.lib.compose.dontCheck super.shh-extras;
    });

  shellEnv = {
    haskellPackages,
    shellPackages,
  }: let
    ghcEnv = shellPackages.ghcWithPackages (p: [
      p.Glob
      p.PyF
      (mkPackage shellPackages {
        src = source [./src/Shhell];
        overrideCabal = {
          buildTarget = "lib:saygo-utils";
          allowInconsistentDependencies = true;
        };
      })
    ]);
  in {
    SHHELL_GHC_ENV = "${ghcEnv}";
    SHHELL_GHCI = "${pkgs.callPackage ./ghci.nix {
      hp = haskellPackages;
      env = ghcEnv;
    }}/bin/ghci";
  };

  programs = {
    haskellPackages,
    env,
    groups,
  }: let
    groupOf = name: lib.findFirst (group: lib.elem name groups.${group}.executables) (lib.removeSuffix ".hs" (baseNameOf executables.${name})) (lib.attrNames groups);
  in
    lib.concatMapAttrs (group: names: let
      extra = groups.${group} or {};
    in
      lib.genAttrs names (_:
        mkPackage haskellPackages {
          src = source ([./src/Bin.hs] ++ map (name: ./. + "/${executables.${name}}") names ++ extra.files or []);
          overrideCabal = {
            pname = "saygo-utils-${group}";
            isLibrary = false;
            buildTarget = toString (map (name: "exe:${name}") names);
            env = env // extra.env or {};
          };
        }))
    (lib.groupBy groupOf (lib.attrNames executables));

  join = {
    programs,
    lazyPackages,
    lazyInstallable,
    passthru,
  }:
    pkgs.symlinkJoin {
      name = "saygo-utils";
      paths = lib.unique (lib.attrValues programs);
      nativeBuildInputs = lib.optional (lazyPackages != []) pkgs.makeWrapper;
      postBuild = lib.optionalString (lazyPackages != []) ''
        wrapProgram $out/bin/comma-bg --set COMMA_BG_LAZY_PACKAGES ${pkgs.writeText "lazy-packages.json" (builtins.toJSON (map (pkg: rec {
            name = lib.getName pkg;
            installable = "${lazyInstallable name}^${(lib.getBin pkg).outputName}";
            program = pkg.meta.mainProgram or name;
          })
          lazyPackages))}
      '';
      inherit passthru;
    };
}
