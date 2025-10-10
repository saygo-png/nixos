{
  description = "Nixos config flake";

  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixos-unstable/nixexprs.tar.xz";
    # nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-unstable-frozen.url = "github:nixos/nixpkgs/cab778239e705082fe97bb4990e0d24c50924c04";

    my-neovim = {
      url = "github:saygo-png/neovim-config";
      inputs = {
        treefmt-nix.follows = "treefmt-nix";
        systems.follows = "systems";
      };
    };

    systems = {
      url = "path:./systems.nix";
      flake = false;
    };

    nixos-hardware.url = "github:nixos/nixos-hardware";

    impermanence.url = "github:nix-community/impermanence";

    disko = {
      url = "github:nix-community/disko";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    drugtracker2 = {
      # url = "git+file:///home/samsepi0l/builds/drugTracker2?ref=compile-time-json-config";
      url = "github:saygo-png/drugTracker2";
      inputs = {
        treefmt-nix.follows = "treefmt-nix";
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
      };
    };

    stylix = {
      url = "github:danth/stylix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";

        nur.follows = "";
        base16-fish.follows = "";
        base16-helix.follows = "";
        base16-vim.follows = "";
        firefox-gnome-theme.follows = "";
        tinted-foot.follows = "";
        tinted-kitty.follows = "";
        tinted-schemes.follows = "";
        tinted-tmux.follows = "";
        tinted-zed.follows = "";
      };
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

    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Zsh plugins {{{
    zsh-autosuggestions = {
      url = "github:zsh-users/zsh-autosuggestions";
      flake = false;
    };

    powerlevel10k = {
      url = "github:romkatv/powerlevel10k";
      flake = false;
    };

    zsh-system-clipboard = {
      url = "github:kutsan/zsh-system-clipboard";
      flake = false;
    };

    zsh-auto-notify = {
      url = "github:MichaelAquilina/zsh-auto-notify";
      flake = false;
    };
    # }}}

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
    pkgsFor = lib.genAttrs (import systems) (system: import nixpkgs {inherit system;});

    treefmtEval = eachSystem (pkgs: inputs.treefmt-nix.lib.evalModule pkgs ./treefmt.nix);
    pkgs-frozen = eachSystem (pkgs: import inputs.nixpkgs-unstable-frozen {inherit (pkgs) system;});
    nixvim-pkgs = eachSystem (pkgs: import inputs.my-neovim.inputs.nixvim.inputs.nixpkgs {inherit (pkgs) system;});

    commonSpecialArgs = system: {
      inherit inputs self;
      nixvim-pkgs = nixvim-pkgs.${system};
      pkgs-frozen = pkgs-frozen.${system};
      lib = nixpkgs.lib.extend (final: _prev: {
        my = import ./modules/lib.nix {
          pkgs = nixpkgs.legacyPackages.${system};
          lib = final;
        };
      });
    };

    commonModules = [
      ./modules/constants.nix
      inputs.disko.nixosModules.disko
      inputs.impermanence.nixosModules.impermanence
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
      }: {_module.args.extraLib = import ./modules/extraLib.nix {inherit config pkgs lib;};})
    ];
  in {
    formatter = eachSystem (pkgs: treefmtEval.${pkgs.system}.config.build.wrapper);
    checks = eachSystem (pkgs: {formatting = treefmtEval.${pkgs.system}.config.build.check self;});

    nixosConfigurations.pc = inputs.nixpkgs.lib.nixosSystem rec {
      system = "x86_64-linux";
      specialArgs =
        {
          conHost = "pc";
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

          ./hosts/pc/hardware-configuration-pc.nix

          inputs.nixos-hardware.nixosModules.common-pc
          inputs.nixos-hardware.nixosModules.common-pc-ssd
          inputs.nixos-hardware.nixosModules.common-cpu-amd
          inputs.nixos-hardware.nixosModules.common-gpu-amd
        ]
        ++ commonModules;
    };
    # install-pc {{{
    # My configurations are so bulky that i struggle with installing them from a
    # usb without OOM errors. This is a minimal host to be deployed on the
    # install target disk. It follows the partitioning schema of corresponding
    # host, which allows for a seamless switch to it.
    nixosConfigurations.install-pc = inputs.nixpkgs.lib.nixosSystem rec {
      system = "x86_64-linux";
      specialArgs =
        {
          conHost = "install-pc";
          conUsername = "samsepi0l";
          conHome = "/home/samsepi0l";
        }
        // (commonSpecialArgs system);

      modules =
        [
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
          conHost = "thinkpad";
          conUsername = "samsepi0l";
          conHome = "/home/samsepi0l";
          conFlakePath = "/home/samsepi0l/nixos";
        }
        // (commonSpecialArgs system);

      modules =
        [
          ./configuration.nix
          ./hosts/thinkpad/thinkpad.nix
          ./hosts/thinkpad/disko-config.nix
          ./hosts/thinkpad/hardware-configuration-thinkpad.nix
          inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x270
          inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
        ]
        ++ commonModules;
    };

    # install-thinkpad {{{
    nixosConfigurations.install-thinkpad = inputs.nixpkgs.lib.nixosSystem rec {
      system = "x86_64-linux";
      specialArgs =
        {
          conHost = "install-thinkpad";
          conUsername = "samsepi0l";
          conHome = "/home/samsepi0l";
        }
        // (commonSpecialArgs system);

      modules =
        [
          ./hosts/thinkpad/disko-config.nix
          ./hosts/thinkpad/install/install.nix
          ./hosts/thinkpad/hardware-configuration-thinkpad.nix
        ]
        ++ commonModules;
    }; # }}}
  };
}
## vim:foldmethod=marker

