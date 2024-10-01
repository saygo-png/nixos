{
  description = "Nixos config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:nixos/nixos-hardware/master";

    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:danth/stylix/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

    nixvim = {
      url = "github:nix-community/nixvim/nixos-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
      inputs.nix-darwin.follows = ""; # no apple here
    };

    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nvim-plugin-cutlass = {
      url = "github:gbprod/cutlass.nvim";
      flake = false;
    };

    nvim-plugin-vim-visual-multi = {
      url = "github:mg979/vim-visual-multi";
      flake = false;
    };

    nvim-plugin-rainbow = {
      url = "github:luochen1990/rainbow";
      flake = false;
    };
  };
  outputs = {
    self,
    nixpkgs,
    ...
  } @ inputs: let
    pkgs-unstable = import inputs.nixpkgs-unstable {system = "x86_64-linux";};
  in {
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {
        inherit inputs self pkgs-unstable;
        host = "nixos";
        conUsername = "samsepi0l";
        conHome = "/home/samsepi0l";
        conFlakePath = "/home/samsepi0l/nixos";
        conFlakePathRel = builtins.toString ./.;
        conAccentColor = "7d8618";
        conRefresh-rate = 144;
        conScreen-width = 1920;
        conScreen-height = 1080;
        conGaps = 6;
        conBorderSize = 1;
      };
      modules = [
        ./hosts/desktop.nix
        ./configuration.nix
        ./resources/static/hardware-configuration.nix
      ];
    };

    nixosConfigurations.thinkpad = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {
        inherit inputs self pkgs-unstable;
        host = "thinkpad";
        conUsername = "samsepi0l";
        conHome = "/home/samsepi0l";
        conFlakePath = "/home/samsepi0l/nixos";
        conFlakePathRel = builtins.toString ./.;
        conAccentColor = "7d8618"; #7d8618 Hacky!!! Add extra color to stylix.
        conRefresh-rate = 60;
        conScreen-width = 1366;
        conScreen-height = 768;
        conGaps = 0;
        conBorderSize = 2;
      };
      modules = [
        inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x270
        ./hosts/thinkpad.nix
        ./configuration.nix
        ./resources/static/hardware-configuration-thinkpad.nix
      ];
    };
  };
}
