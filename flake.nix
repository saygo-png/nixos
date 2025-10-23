{
  description = "NixOS config flake";

  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixos-unstable/nixexprs.tar.xz";
    # nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-unstable-frozen.url = "github:nixos/nixpkgs/d7f52a7a640bc54c7bb414cca603835bf8dd4b10";

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
      inputs.nixpkgs.follows = "nixpkgs";
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
    ...
  } @ inputs: let
    inherit (nixpkgs) lib;
    inherit (builtins) filter;

    eachSystem = f: lib.genAttrs (import systems) (system: f pkgsFor.${system});
    pkgsFor = lib.genAttrs (import systems) (system: import nixpkgs {inherit system;});

    treefmtEval = eachSystem (pkgs: inputs.treefmt-nix.lib.evalModule pkgs ./treefmt.nix);
    pkgs-frozen = eachSystem (pkgs: import inputs.nixpkgs-unstable-frozen {inherit (pkgs) system;});
    nixvim-pkgs = eachSystem (pkgs: import inputs.my-neovim.inputs.nixvim.inputs.nixpkgs {inherit (pkgs) system;});

    commonSpecialArgs = system: {
      inherit inputs self;
      nixvim-pkgs = nixvim-pkgs.${system};
      pkgs-frozen = pkgs-frozen.${system};
      lib = lib.extend (final: _: {
        my = import ./modules/lib.nix {
          pkgs = nixpkgs.${system};
          lib = final;
        };
      });
    };

    commonModules = [
      inputs.disko.nixosModules.disko
      inputs.impermanence.nixosModules.impermanence
      {
        options.warnings = lib.mkOption {
          apply = filter (w: !(lib.hasInfix "If multiple of these password options are set at the same time" w));
        };
      }

      ({
        lib,
        config,
        pkgs,
        ...
      }: {_module.args.extraLib = import ./modules/extraLib.nix {inherit config pkgs lib;};})
    ];

    mkSystem = name: uniqueModules:
      lib.nixosSystem rec {
        system = "x86_64-linux";
        specialArgs =
          {
            conHost = name;
            conUsername = "samsepi0l";
            conHome = "/home/samsepi0l";
            conFlakePath = "/home/samsepi0l/nixos";
          }
          // (commonSpecialArgs system);
        modules =
          [
            ./configuration.nix
            (./hosts + "/${name}/${name}.nix")
            (./hosts + "/${name}/disko-config.nix")
            (./hosts + "/${name}/hardware-configuration.nix")
          ]
          ++ commonModules
          ++ uniqueModules;
      };

    mkInstall = host:
      lib.nixosSystem {
        inherit (host.pkgs) system;
        specialArgs =
          host._module.specialArgs // {conHost = "install-" + host._module.specialArgs.conHost;};

        modules =
          [
            (./hosts + "/${host._module.specialArgs.conHost}/install/install.nix")
            (./hosts + "/${host._module.specialArgs.conHost}/hardware-configuration.nix")
            (./hosts + "/${host._module.specialArgs.conHost}/disko-config.nix")
          ]
          ++ commonModules;
      };
  in {
    formatter = eachSystem (pkgs: treefmtEval.${pkgs.system}.config.build.wrapper);
    checks = eachSystem (pkgs: {formatting = treefmtEval.${pkgs.system}.config.build.check self;});
    nixosConfigurations = rec {
      install-pc = mkInstall pc;
      pc = mkSystem "pc" [
        inputs.nixos-hardware.nixosModules.common-pc
        inputs.nixos-hardware.nixosModules.common-pc-ssd
        inputs.nixos-hardware.nixosModules.common-cpu-amd
        inputs.nixos-hardware.nixosModules.common-gpu-amd
      ];

      install-thinkpad = mkInstall thinkpad;
      thinkpad = mkSystem "thinkpad" [
        inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x270
        inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
      ];
    };
  };
}
