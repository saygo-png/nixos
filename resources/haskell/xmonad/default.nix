{pkgs}:
pkgs.haskellPackages.developPackage {
  root =
    pkgs.nix-gitignore.gitignoreSourcePure
    [
      "dist-newstyle"
      ".*#"
      ".git"
    ]
    ./.;
  returnShellEnv = false;
}
