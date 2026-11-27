{
  niceHaskell,
  pkgs,
  processSrc,
  haskellPackages ? pkgs.haskellPackages,
  # Given to comma-bg as JSON, with the installable it evaluates for each one.
  lazyPackages ? [],
  lazyInstallable ? null,
  ...
}: let
  saygo = import ./lib.nix {inherit niceHaskell pkgs;};

  shellEnv = saygo.shellEnv {
    inherit haskellPackages;
    shellPackages = saygo.withProcess processSrc haskellPackages;
  };

  # Each program is built on its own, from the cabal file, its own source and
  # src/Bin.hs, so changing one rebuilds only that one. shhell's helpers are
  # built with it, since it finds them next to itself.
  programs = saygo.programs {
    inherit haskellPackages;
    env.RUNTIME_INPUTS = saygo.runtimeInputs;
    groups.shhell = {
      executables = ["shhell" "ghc-link-cache" "shhell-bench"];
      files = [./src/Shhell];
      env = shellEnv;
    };
  };
in
  saygo.join {
    inherit programs lazyPackages lazyInstallable;
    passthru = {
      inherit programs shellEnv;
      inherit (saygo) runtimeInputs;
      inherit (saygo.mkPackage haskellPackages {}) env;
    };
  }
