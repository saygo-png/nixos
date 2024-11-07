# From nixpkgs shell.nix
# Note: We intentionally don't use Flakes here,
# because every time you change any file and do another `nix develop`,
# it would create another copy of the entire ~500MB tree in the store.
# See https://github.com/NixOS/nix/pull/6530 for the future
{
  system ? builtins.currentSystem,
  nixpkgs ? null,
}: let
  pinnedNixpkgs = builtins.fromJSON (builtins.readFile ./ci/pinned-nixpkgs-for-nixshell.json);
  nixpkgs' =
    if nixpkgs == null
    then
      fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/${pinnedNixpkgs.nixpkgs.rev}.tar.gz";
        sha256 = pinnedNixpkgs.nixpkgs.sha256;
      }
    else nixpkgs;

  pkgs = import nixpkgs' {
    inherit system;
    config = {};
    overlays = [];
  };
in
  pkgs.mkShell {
    packages = [
      pkgs.devenv
      pkgs.hello
      pkgs.pridefetch
    ];
  }
