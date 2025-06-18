{pkgs ? import <nixpkgs> {}}:
pkgs.mkShell {
  packages = with pkgs; [
    (callPackage ./default.nix {})
  ];
}
