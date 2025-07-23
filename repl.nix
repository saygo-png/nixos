# Taken from https://github.com/Michael-C-Buckley/nixos/blob/master/repl.nix
# Preload the Repl with various aliases and functions, see for more info:
# https://bmcgee.ie/posts/2023/01/nix-and-its-slow-feedback-loop/#how-you-should-use-the-repl
# Special thanks to Iynaix for the inspiration:
# https://github.com/iynaix/dotfiles/blob/main/repl.nix
{host ? "pc"}: let
  inherit (builtins) getFlake;
  flake = getFlake (toString ./.);

  hosts = builtins.attrNames flake.nixosConfigurations;
in
  rec {
    inherit flake;
    inherit (flake) inputs self;
    inherit (flake.inputs) nixpkgs;
    inherit (flake.nixosConfigurations.${host}) lib;

    c = flake.nixosConfigurations.${host}.config;

    # Aliases to quickly get the configs of my defined systems
    config = builtins.listToAttrs (map (name: {
        inherit name;
        value = flake.nixosConfigurations.${name}.config;
      })
      hosts);
  }
  // flake
