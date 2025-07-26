{pkgs}:
pkgs.haskellPackages.developPackage {
  root = ./.;
  returnShellEnv = false;
}
