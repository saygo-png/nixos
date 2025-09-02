{pkgs}: let
  pkg = pkgs.haskellPackages.developPackage {
    root = ./.;
    returnShellEnv = false;
  };
in
  pkgs.haskellPackages.generateOptparseApplicativeCompletions ["convertlink"] pkg
