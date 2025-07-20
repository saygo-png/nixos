{pkgs}: let
  hl = pkgs.haskell.lib;

  listSwitchFunc = [
    {
      switch = true;
      function = hl.buildStrictly;
    }
  ];

  onSwitchApplyFunc = set: object:
    if set.switch
    then set.function object
    else object;

  pkg = pkgs.haskellPackages.developPackage {
    root = ./.;
    returnShellEnv = false;

    modifier = drv:
      hl.overrideCabal drv (_: {
        doHaddock = false;
        enableDeadCodeElimination = true;
        passthru.nixpkgs = pkgs;
      });
  };
in
  pkgs.lib.foldr onSwitchApplyFunc pkg listSwitchFunc
