{
  description = "Nixos config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixvim = {
      url = "github:nix-community/nixvim/nixos-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
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
        inherit inputs pkgs-unstable;
        host = "nixos";
      };
      modules = [
        ./configuration.nix
        ./resources/static/hardware-configuration.nix
      ];
    };
    nixosConfigurations.nixosExternalDrive = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {
        inherit inputs pkgs-unstable;
        host = "nixosExternalDrive";
      };
      modules = [
        ./configuration.nix
        ./resources/static/hardware-configuration-ExternalDrive.nix
      ];
    };
  };
}
