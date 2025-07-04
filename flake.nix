{
  description = "Nixos config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    # nixpkgs-unstable-frozen.url = "github:nixos/nixpkgs/28b5b8af91ffd2623e995e20aee56510db49001a";
    carla-fix.url = "github:ArchercatNEO/nixpkgs/pyliblo-drop";

    nixos-hardware = {
      url = "github:nixos/nixos-hardware";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.treefmt-nix.follows = "treefmt-nix";
    };

    impermanence.url = "github:nix-community/impermanence";

    disko = {
      url = "github:nix-community/disko";
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

    # Mine {{{
    extras-nixos = {
      url = "github:saygo-png/extrasNixos";
      flake = false;
    };

    cookiecutter-templates = {
      url = "github:saygo-png/cookiecutter-templates";
      flake = false;
    };

    zlequalizer = {
      url = "github:saygo-png/ZLEqualizer-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # }}}

    gruvbox-kvantum = {
      url = "github:sachnr/gruvbox-kvantum-themes";
      flake = false;
    };

    gruvbox-vesktop = {
      url = "github:shvedes/discord-gruvbox";
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

    devenv = {
      url = "github:cachix/devenv";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    treefmt-nix.url = "github:numtide/treefmt-nix";

    # Small utilities {{{
    format-udf = {
      url = "github:JElchison/format-udf";
      flake = false;
    };

    vmrss = {
      url = "github:ThePrimeagen/vmrss";
      flake = false;
    };
    # }}}

    # Awesome {{{
    awesome-git = {
      url = "github:awesomeWM/awesome";
      flake = false;
    };

    awesome-lain = {
      url = "github:lcpz/lain";
      flake = false;
    };
    # }}}

    # Neovim {{{
    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nvim-plugin-cutlass = {
      url = "github:gbprod/cutlass.nvim";
      flake = false;
    };

    nvim-plugin-rainbow = {
      url = "github:luochen1990/rainbow";
      flake = false;
    };

    nvim-plugin-vim-visual-multi = {
      url = "github:mg979/vim-visual-multi";
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
    # }}}
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
      lib = nixpkgs.lib.extend (final: _prev: {
        my = import ./modules/myLib.nix {
          pkgs = nixpkgs.legacyPackages.${system};
          lib = final;
        };
      });
    };

    commonModules = [
      ./modules/myConstants.nix
      (_: {
        options = {
          warnings = lib.mkOption {
            apply = builtins.filter (w: !(lib.hasInfix "If multiple of these password options are set at the same time" w));
          };
        };
      })

      ({
        lib,
        config,
        pkgs,
        ...
      }: {_module.args.extraLib = import ./modules/myExtraLib.nix {inherit config pkgs lib;};})
    ];
  in {
    formatter = eachSystem (pkgs: treefmtEval.${pkgs.system}.config.build.wrapper);
    checks = eachSystem (pkgs: {formatting = treefmtEval.${pkgs.system}.config.build.check self;});

    nixosConfigurations.pc = inputs.nixpkgs.lib.nixosSystem rec {
      system = "x86_64-linux";
      specialArgs =
        {
          conUsername = "samsepi0l";
          conHome = "/home/samsepi0l";
          conFlakePath = "/home/samsepi0l/nixos";
        }
        // (commonSpecialArgs system);
      modules =
        [
          ./configuration.nix
          ./hosts/pc/pc.nix
          ./hosts/pc/disko-config.nix

          inputs.disko.nixosModules.disko
          inputs.impermanence.nixosModules.impermanence
          ./hosts/pc/hardware-configuration-pc.nix

          inputs.nixos-hardware.nixosModules.common-pc
          inputs.nixos-hardware.nixosModules.common-pc-ssd
          inputs.nixos-hardware.nixosModules.common-cpu-amd
          inputs.nixos-hardware.nixosModules.common-gpu-amd
        ]
        ++ commonModules;
    };
    # install-pc {{{
    # My configuration is so bulky that i struggle with installing it from a
    # usb without OOM errors. This is a minimal host to be deployed on the
    # install target disk. It follows the partitioning schema of the "pc"
    # host, which allows for a seamless switch to it.
    nixosConfigurations.install-pc = inputs.nixpkgs.lib.nixosSystem rec {
      system = "x86_64-linux";
      specialArgs =
        {
          conUsername = "samsepi0l";
          conHome = "/home/samsepi0l";
        }
        // (commonSpecialArgs system);

      modules =
        [
          inputs.disko.nixosModules.disko
          inputs.impermanence.nixosModules.impermanence
          ./hosts/pc/install/install.nix
          ./hosts/pc/hardware-configuration-pc.nix
          ./hosts/pc/disko-config.nix
        ]
        ++ commonModules;
    }; # }}}

    nixosConfigurations.thinkpad = inputs.nixpkgs.lib.nixosSystem rec {
      system = "x86_64-linux";
      specialArgs =
        {
          conUsername = "samsepi0l";
          conHome = "/home/samsepi0l";
          conFlakePath = "/home/samsepi0l/nixos";
        }
        // (commonSpecialArgs system);

      modules =
        [
          ./configuration.nix
          ./hosts/thinkpad/thinkpad.nix
          ./hosts/thinkpad/hardware-configuration-thinkpad.nix
          inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x270
          inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
        ]
        ++ commonModules;
    };
  };
}
## vim:foldmethod=marker
