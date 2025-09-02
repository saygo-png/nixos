{
  #################################################################################################################
  # File taken from https://github.com/haskell-nix/hnix/blob/3f334d9b5a8b1044df850167b1b459e5649868f3/default.nix #
  #################################################################################################################
  # For current default and explicitly supported GHCs https://search.nixos.org/packages?query=ghc&from=0&size=500&channel=unstable, Nixpkgs implicitly supports older minor versions also, until the configuration departs from compatibility with them.
  compiler ? "ghc984",
  # Default.nix is a unit package abstraciton that allows to abstract over packages even in monorepos:
  # Example: pass --arg cabalName --arg packageRoot "./subprojectDir", or map default.nix over a list of tiples for subprojects.
  # cabalName is package resulting name: by default and on error resolves in haskellPackages.developPackage to project root directory name by default, but outside the haskellPackages.developPackage as you see below packageRoot can be different
  cabalName ? "saygo-utils",
  packageRoot ? pkgs.nix-gitignore.gitignoreSource [] ./.,
  # This settings expose most of the Nixpkgs Haskell.lib API: https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/lib.nix
  # Some of these options implicitly enable other options they require, and some counterpoint options clash, obviously
  # Don't fail at configure time if there are multiple versions of the same package in the (recursive) dependencies of the package being built. Will delay failures, if any, to compile time.
  allowInconsistentDependencies ? false,
  # Escape the version bounds from the cabal file. You may want to avoid this function.
  doJailbreak ? false,
  # Nix dependency checking, compilation and execution of test suites listed in the package description file.
  doCheck ? false,
  # Just produce a SDist src tarball
  sdistTarball ? false,
  # The strict packaging process as used on Hackage. Tests consistency of the Cabal file.
  buildFromSdist ? true,
  # Allow a shell environment to be requested
  returnShellEnv ? false,
  # Turn all warn into err with {-Wall,-Werror}
  failOnAllWarnings ? false,
  # `failOnAllWarnings` + `buildFromSdist`
  buildStrictly ? false,
  #  2020-06-02: NOTE: enableDeadCodeElimination = true: On GHC =< 8.8.3 macOS build falls due to https://gitlab.haskell.org/ghc/ghc/issues/17283
  enableDeadCodeElimination ? true,
  # Disabled GHC code optimizations make build/tolling/dev loops faster.
  # Works also for Haskel IDE Engine and GHCID.
  # Enable optimizations for production use, and to pass benchmarks.
  disableOptimization ? false,
  # Use faster `gold` ELF linker from GNU binutils instead of older&slower but more versatile GNU linker. Is not available by default since macOS does not have it.
  linkWithGold ? true,
  # Provide an inventory of performance events and timings for the execution. Provides information in an absolute sense. Nothing is timestamped.
  enableLibraryProfiling ? false,
  enableExecutableProfiling ? false,
  # Include tracing information & abilities. Tracing records the chronology, often with timestamps and is extensive in time
  doTracing ? false,
  # Include DWARF debugging information & abilities
  enableDWARFDebugging ? false,
  # Strip results from all debugging symbols
  doStrip ? true,
  # Nixpkgs expects shared libraries
  enableSharedLibraries ? true,
  # Ability to make static libraries
  enableStaticLibraries ? false,
  # Make hybrid executable that is also a shared library
  enableSharedExecutables ? false,
  # link executables statically against haskell libs to reduce closure size
  justStaticExecutables ? false,
  enableSeparateBinOutput ? false,
  # checkUnusedPackages: is `failOnAllWarnings` + `cabal sdist` + post-build dep check.
  # Currently uses `packunused` or GHC 8.8 internals, later switches into GHC internal feature.
  # Adds a post-build check to verify that dependencies declared in the cabal file are actually used.
  checkUnusedPackages ? false,
  # Generation and installation of haddock API documentation
  doHaddock ? false,
  #	Generate hyperlinked source code for documentation using HsColour, and have Haddock documentation link to it.
  doHyperlinkSource ? false,
  # Generation and installation of a coverage report. See https://wiki.haskell.org/Haskell_program_coverage
  doCoverage ? false,
  # doBenchmark: Dependency checking + compilation and execution for benchmarks listed in the package description file.
  doBenchmark ? false,
  # For binaries named in `executableNamesToShellComplete` list, generate and bundle-into package an automatically loaded shell complettions
  generateOptparseApplicativeCompletions ? false,
  executableNamesToShellComplete ? [""],
  # Include Hoogle executable and DB into derivation
  withHoogle ? false,
  pkgs,
}: let
  hlib = pkgs.haskell.lib;
  inherit (pkgs) lib;

  hpkgs = pkgs.haskell.packages.${compiler};
  makeCompletions = hpkgs.generateOptparseApplicativeCompletions executableNamesToShellComplete;

  # Application of functions from this list to the package in code here happens
  # in the reverse order (from the tail). Some options depend on & override others,
  # so if enabling options caused Nix error or not expected result - change the order,
  # and please do not change this order without proper testing.
  mkSwitch = switch: function: {inherit switch function;};
  listSwitchFunc = [
    (mkSwitch sdistTarball hlib.sdistTarball)
    (mkSwitch buildFromSdist hlib.buildFromSdist)
    (mkSwitch buildStrictly hlib.buildStrictly)
    (mkSwitch disableOptimization hlib.disableOptimization)
    (mkSwitch doJailbreak hlib.doJailBreak)
    (mkSwitch doStrip hlib.doStrip)
    (mkSwitch enableDWARFDebugging hlib.enableDWARFDebugging)
    (mkSwitch linkWithGold hlib.linkWithGold)
    (mkSwitch failOnAllWarnings hlib.failOnAllWarnings)
    (mkSwitch justStaticExecutables hlib.justStaticExecutables)
    (mkSwitch checkUnusedPackages (hlib.checkUnusedPackages {}))
    (mkSwitch generateOptparseApplicativeCompletions makeCompletions)
    (mkSwitch doHyperlinkSource hlib.doHyperlinkSource)
  ];

  onSwitchApplyFunc = set: object:
    if set.switch
    then set.function object
    else object;

  package = hpkgs.developPackage {
    name = cabalName;
    root = packageRoot;

    modifier = drv:
      hlib.overrideCabal drv (old: {
        buildTools = (old.buildTools or []) ++ [pkgs.makeWrapper];

        configureFlags =
          (old.configureFlags or [])
          ++ lib.optional (!disableOptimization) [
            "--ghc-options=-O2"
            "--enable-optimization=2"
            "--enable-split-sections"
            "--enable-executable-stripping"
          ]
          ++ lib.optional doTracing "--flags=tracing";

        # postInstall =
        #   (old.postInstall or "")
        #   + ''
        #     wrapProgram $out/bin/drug \
        #       --prefix PATH : ${pkgs.lib.makeBinPath [pkgs.libnotify]}
        #   '';

        inherit allowInconsistentDependencies;
        inherit doCheck;
        inherit enableDeadCodeElimination;
        inherit enableLibraryProfiling;
        inherit enableExecutableProfiling;
        inherit enableSharedLibraries;
        inherit enableStaticLibraries;
        inherit enableSharedExecutables;
        inherit enableSeparateBinOutput;
        inherit doBenchmark;
        inherit doCoverage;
        inherit doHaddock;

        passthru.nixpkgs = pkgs;
      });

    inherit returnShellEnv withHoogle;
  };
  composedPackage = lib.foldr onSwitchApplyFunc package listSwitchFunc;
in
  if returnShellEnv
  then package
  else composedPackage
