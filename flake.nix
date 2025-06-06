{
  description = "Nixos config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:nixos/nixos-hardware";
    # nixpkgs-unstable-frozen.url = "github:nixos/nixpkgs/28b5b8af91ffd2623e995e20aee56510db49001a";

    zlequalizer = {
      url = "github:saygo-png/ZLEqualizer-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    extras-nixos = {
      url = "github:saygo-png/extrasNixos";
      flake = false;
    };

    firefox-addons = {
      url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    firefox-onebar = {
      url = "git+https://git.gay/freeplay/Firefox-Onebar.git";
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
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
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
    systems,
    home-manager,
    ...
  } @ inputs: let
    lib = nixpkgs.lib // home-manager.lib;
    eachSystem = f: lib.genAttrs (import systems) (system: f pkgsFor.${system});
    pkgsFor = lib.genAttrs (import systems) (system:
      import nixpkgs {
        inherit system;
      });

    treefmtEval = eachSystem (pkgs: inputs.treefmt-nix.lib.evalModule pkgs ./treefmt.nix);
    pkgs-unstable = import inputs.nixpkgs-unstable {system = "x86_64-linux";};

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
      ./configuration.nix
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
      system = "x86_64-linux";
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
          ./hosts/desktop/desktop.nix
          ./hosts/desktop/hardware-configuration-desktop.nix
          inputs.nixos-hardware.nixosModules.common-pc
          inputs.nixos-hardware.nixosModules.common-pc-ssd
          inputs.nixos-hardware.nixosModules.common-cpu-amd
          inputs.nixos-hardware.nixosModules.common-gpu-amd
        ]
        ++ commonModules;
    };

    nixosConfigurations.thinkpad = inputs.nixpkgs.lib.nixosSystem rec {
      system = "x86_64-linux";
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
          ./hosts/thinkpad/thinkpad.nix
          ./hosts/thinkpad/hardware-configuration-thinkpad.nix
          inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x270
          inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
        ]
        ++ commonModules;
    };
  };
}
