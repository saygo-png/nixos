{
  description = "Nixos config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-24.11"; #I don't need unstable, so i set it to stable for now

    nixos-hardware.url = "github:nixos/nixos-hardware/master";
    # nixpkgs-unstable-frozen.url = "github:nixos/nixpkgs/28b5b8af91ffd2623e995e20aee56510db49001a";

    zlequalizer = {
      url = "github:saygo-png/ZLEqualizer-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    extras-nixos = {
      url = "github:saygo-png/extrasNixos";
      flake = false;
    };

    mpv-intersubs = {
      url = "github:oltodosel/interSubs";
      flake = false;
    };

    format-udf = {
      url = "github:JElchison/format-udf";
      flake = false;
    };

    vmrss = {
      url = "github:ThePrimeagen/vmrss";
      flake = false;
    };

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

    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    treefmt-nix.url = "github:numtide/treefmt-nix";

    awesome-git = {
      url = "github:awesomeWM/awesome";
      flake = false;
    };

    awesome-lain = {
      url = "github:lcpz/lain";
      flake = false;
    };

    nvim-plugin-tshjkl = {
      url = "github:gsuuon/tshjkl.nvim";
      flake = false;
    };

    nvim-plugin-unfocused-cursor = {
      url = "github:amarz45/nvim-unfocused-cursor";
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

  outputs = {
    self,
    nixpkgs,
    ...
  } @ inputs: let
    allPossibleSystems = [
      "aarch64-linux"
      "i686-linux"
      "x86_64-linux"
      "aarch64-darwin"
      "x86_64-darwin"
    ];
    eachSystem = f: nixpkgs.lib.genAttrs allPossibleSystems (system: f nixpkgs.legacyPackages.${system});
    treefmtEval = eachSystem (pkgs: inputs.treefmt-nix.lib.evalModule pkgs ./treefmt.nix);

    pkgs-unstable = import inputs.nixpkgs-unstable {system = "x86_64-linux";};

    mySystem = "x86_64-linux";
    commonSpecialArgs = system: {
      inherit inputs self pkgs-unstable;
      # inherit nixpkgs-unstable-frozen;
      conFlakePathRel = builtins.toString ./.;
      lib = nixpkgs.lib.extend (final: _prev: {
        my = import ./modules/myLib.nix {
          pkgs = nixpkgs.legacyPackages.${system};
          lib = final;
        };
      });
    };
    commonModules = [
      ({
        lib,
        config,
        pkgs,
        ...
      }: {
        _module.args.extraLib = import ./modules/myExtraLib.nix {
          inherit config pkgs lib;
        };
      })
    ];
  in {
    formatter = eachSystem (pkgs: treefmtEval.${pkgs.system}.config.build.wrapper);
    checks = eachSystem (pkgs: {formatting = treefmtEval.${pkgs.system}.config.build.check self;});
    nixosConfigurations.nixos = inputs.nixpkgs.lib.nixosSystem rec {
      system = mySystem;
      specialArgs =
        {
          host = "nixos";
          conUsername = "samsepi0l";
          conHome = "/home/samsepi0l";
          conFlakePath = "/home/samsepi0l/nixos";
        }
        // (commonSpecialArgs system);
      modules =
        [
          inputs.nixos-hardware.nixosModules.common-pc
          inputs.nixos-hardware.nixosModules.common-pc-ssd
          inputs.nixos-hardware.nixosModules.common-gpu-amd
          inputs.nixos-hardware.nixosModules.common-cpu-amd
          ./configuration.nix
          ./hosts/desktop/desktop.nix
          ./hosts/desktop/hardware-configuration-desktop.nix
        ]
        ++ commonModules;
    };

    nixosConfigurations.thinkpad = inputs.nixpkgs.lib.nixosSystem rec {
      system = mySystem;
      specialArgs =
        {
          host = "thinkpad";
          conUsername = "samsepi0l";
          conHome = "/home/samsepi0l";
          conFlakePath = "/home/samsepi0l/nixos";
        }
        // (commonSpecialArgs system);

      modules =
        [
          inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x270
          inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
          ./configuration.nix
          ./hosts/thinkpad/thinkpad.nix
          ./hosts/thinkpad/hardware-configuration-thinkpad.nix
        ]
        ++ commonModules;
    };
  };
}
