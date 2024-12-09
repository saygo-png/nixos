{
  description = "Nixos config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-unstable-working-krita.url = "github:nixos/nixpkgs/28b5b8af91ffd2623e995e20aee56510db49001a";
    nixos-hardware.url = "github:nixos/nixos-hardware/master";

    devenv = {
      url = "github:cachix/devenv";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:danth/stylix/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

    nixvim = {
      url = "github:nix-community/nixvim/nixos-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
      inputs.nix-darwin.follows = ""; # no apple here
    };

    awesome-git = {
      url = "github:awesomeWM/awesome";
      flake = false;
    };

    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nvim-plugin-tshjkl = {
      url = "github:gsuuon/tshjkl.nvim";
      flake = false;
    };

    nvim-plugin-faster = {
      url = "github:pteroctopus/faster.nvim";
      flake = false;
    };

    nvim-plugin-telescope-git-file-history = {
      url = "github:isak102/telescope-git-file-history.nvim";
      flake = false;
    };
  };
  outputs = {self, ...} @ inputs: let
    pkgs-unstable = import inputs.nixpkgs-unstable {system = "x86_64-linux";};
    nixpkgs-unstable-working-krita = import inputs.nixpkgs-unstable-working-krita {system = "x86_64-linux";};
    system = "x86_64-linux";
  in {
    nixosConfigurations.nixos = inputs.nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = {
        inherit inputs self pkgs-unstable nixpkgs-unstable-working-krita;
        host = "nixos";
        conUsername = "samsepi0l";
        conHome = "/home/samsepi0l";
        conFlakePath = "/home/samsepi0l/nixos";
        conFlakePathRel = builtins.toString ./.;
      };
      modules = [
        inputs.nixos-hardware.nixosModules.common-pc
        inputs.nixos-hardware.nixosModules.common-pc-ssd
        inputs.nixos-hardware.nixosModules.common-gpu-amd
        inputs.nixos-hardware.nixosModules.common-cpu-amd
        ./configuration.nix
        ./hosts/desktop/desktop.nix
        ./hosts/desktop/hardware-configuration-desktop.nix
      ];
    };

    nixosConfigurations.thinkpad = inputs.nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = {
        inherit inputs self pkgs-unstable nixpkgs-unstable-working-krita;
        host = "thinkpad";
        conUsername = "samsepi0l";
        conHome = "/home/samsepi0l";
        conFlakePath = "/home/samsepi0l/nixos";
        conFlakePathRel = builtins.toString ./.;
      };

      modules = [
        inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x270
        inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
        ./configuration.nix
        ./hosts/thinkpad/thinkpad.nix
        ./hosts/thinkpad/hardware-configuration-thinkpad.nix
      ];
    };
  };
}
