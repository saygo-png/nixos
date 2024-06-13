let
  constants = {
    hostname = "nixos"; # Change manually in flake.nix
    username = "samsepi0l";
    home = "/home/samsepi0l";
    flake-path = "/home/samsepi0l/nixos";
    system = "x86_64-linux";
    version = "24.05";
  };
in
  {
    pkgs,
    lib,
    # config,
    inputs,
    ...
  }: {
    imports = [
      ./hardware-configuration.nix
      inputs.stylix.nixosModules.stylix
      inputs.home-manager.nixosModules.default
    ];
    # Use the systemd-boot EFI boot loader.
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    networking.hostName = "${constants.hostname}"; # Define your hostname.
    networking.networkmanager.enable = true;

    # Locale.
    i18n.defaultLocale = "en_US.UTF-8";
    time.timeZone = "Europe/Warsaw";
    services.xserver.xkb = {
      layout = "pl";
      options = "caps:escape";
    };
    console = {
      useXkbConfig = true;
      font = "Lat2-Terminus16";
    };

    # Define a user account. Don't forget to set a password with ‘passwd $USERNAME’.
    users.users.samsepi0l = {
      isNormalUser = true;
      extraGroups = ["wheel" "networkmanager"]; # Enable ‘sudo’ for the user.
      shell = pkgs.fish;
    };

    #########
    # NixOs #
    #########

    nix.settings.experimental-features = ["nix-command" "flakes"];
    nix.settings.auto-optimise-store = true;
    boot.loader.grub.configurationLimit = 30;
    boot.loader.systemd-boot.configurationLimit = 30;
    nix.gc = {
      automatic = true;
      dates = "2day";
      options = "--delete-older-than 15d";
    };

    ############
    # Services #
    ############

    # Enable the X11 windowing system.
    services.xserver.enable = true;

    # Enable the GNOME Desktop Environment.
    services.xserver.displayManager.gdm.enable = true;
    services.xserver.desktopManager.gnome.enable = true;
    programs.hyprland.enable = true;
    # Enable sound.
    hardware.pulseaudio.enable = false;
    services.pipewire = {
      enable = true;
      pulse.enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      extraConfig.pipewire."92-low-latency" = {
        context.properties = {
          default.clock.rate = 48000;
          default.clock.quantum = 8;
          default.clock.min-quantum = 8;
          default.clock.max-quantum = 8;
        };
      };
    };

    # Disable touchpad support (enabled default in most desktops).
    services.libinput.enable = false;

    #########
    # Shell #
    #########

    environment.shellAliases = {
      "ls" = "${pkgs.eza}/bin/eza";
      "rt" = "${pkgs.trashy}/bin/trash";
      # "cd" = "${pkgs.zoxide}/bin/z";
      "cd" = "z";
    };

    environment.sessionVariables = {
      FLAKE = "${constants.flake-path}"; # For nix helper.
    };

    environment.systemPackages = with pkgs; [
      # Nix.
      nh
      nvd
      nix-output-monitor

      # Other.
      helix
      wget
      eza
      trashy
      tmux
      dash
      git
      xdg-utils
      xdg-desktop-portal
      xdg-desktop-portal-gtk
      xdg-desktop-portal-hyprland
      libnotify

      # GUI.
      rofi-wayland

      # Shellscripts.
      (writeShellScriptBin
        "hard-clean-nix"
        ''
          nix-collect-garbage --delete-older-than 5d
          sudo nix-collect-garbage --delete-older-than 5d
          nix store optimise
          sudo nix store optimise
        '')
    ];

    #########
    # OTHER #
    #########

    programs.fish.enable = true;

    stylix.enable = true;
    stylix.autoEnable = true;
    stylix.polarity = "dark";
    stylix.image = ./resources/wallpaper.png;
    stylix.targets.grub.useImage = true;
    stylix.targets.nixvim.transparent_bg.main = true;
    stylix.targets.nixvim.transparent_bg.sign_column = true;
    stylix.cursor.package = pkgs.capitaine-cursors-themed;
    stylix.cursor.name = "Capitaine Cursors (Gruvbox)";
    stylix.base16Scheme = {
      base00 = "282828"; # dark  ----
      base01 = "3c3836"; # dark  ---
      base02 = "504945"; # dark  --
      base03 = "665c54"; # dark  -
      base04 = "bdae93"; # light +
      base05 = "d5c4a1"; # light ++
      base06 = "ebdbb2"; # light +++
      base07 = "fbf1c7"; # light ++++
      base08 = "fb4934"; # red
      base09 = "fe8019"; # orange
      base0A = "fabd2f"; # yellow
      base0B = "b8bb26"; # green
      base0C = "8ec07c"; # aqua/cyan
      base0D = "83a598"; # blue
      base0E = "d3869b"; # purple
      base0F = "d65d0e"; # brown
    };
    stylix.fonts = {
      monospace = {
        package = pkgs.courier-prime;
        name = "Courier Prime";
      };
      sansSerif = {
        package = pkgs.courier-prime;
        name = "Courier Prime";
      };
      serif = {
        package = pkgs.courier-prime;
        name = "Courier Prime";
      };
      emoji = {
        package = pkgs.noto-fonts-emoji;
        name = "Noto Color Emoji";
      };
    };

    stylix.fonts.sizes = {
      applications = 12;
      terminal = 13;
      desktop = 10;
      popups = 10;
    };

    stylix.opacity = {
      applications = 0.8;
      terminal = 0.8;
      desktop = 0.8;
      popups = 0.8;
    };

    ################
    # HOME MANAGER #
    ################

    home-manager = {
      extraSpecialArgs = {inherit inputs;};
      backupFileExtension = "backup"; # h-m breaks without it.

      users.samsepi0l = {
        imports = [
          inputs.nixvim.homeManagerModules.nixvim
        ];
        home = {
          username = "samsepi0l";
          homeDirectory = "/home/samsepi0l";
          stateVersion = "24.05"; # Dont change

          packages = with pkgs; [
            # GUI.
            librewolf
            keepassxc
            rhythmbox
            inkscape
            krita

            # Tooling.
            nil # Nix lsp
            alejandra # Nix formatter

            # Command line.
            gallery-dl
            zoxide
            btop
            iamb
            cbonsai
            neofetch
            termdown
            tldr
            htop
            moar
          ];

          sessionVariables = {
            # Default programs.
            PAGER = "${pkgs.moar}/bin/moar";
            # Systemd is retarded and doesnt use normal pager variable :DDDDD
            SYSTEMD_PAGER = "${pkgs.moar}/bin/moar";
            OPENER = "${pkgs.xdg-utils}/bin/xdg-open";
            VISUAL = "nvim";
            EDITOR = "nvim";
            SUDO_EDITOR = "nvim";
            TERMINAL = "${pkgs.alacritty}/bin/alacritty";
            TERMINAL_PROG = "${pkgs.alacritty}/bin/alacritty";
            BROWSER = "${pkgs.librewolf}/bin/librewolf";
            # Firefox hardware decode.
            MOZ_X11_EGL = 1;
            NO_AT_BRIDGE = 1;
            # Unreal engine .net cli tool turn off telemetry.
            DOTNET_CLI_TELEMETRY_OPTOUT = "true";
          };
        };
        # Development, internal.
        programs.fish.enable = true;
        programs.zoxide.enable = true;
        programs.home-manager.enable = true;
        programs.git = {
          enable = true;
          package = pkgs.gitAndTools.gitFull;
          userName = "saygo-png";
          userEmail = "saygo.mail@proton.me";
          aliases = {
            undo = "reset HEAD~1 --mixed";
            amend = "commit -a --amend";
          };
          extraConfig = {
            color = {
              ui = "auto";
            };
            diff = {
              tool = "vimdiff";
              mnemonicprefix = true;
            };
            merge = {
              tool = "splice";
            };
            push = {
              default = "simple";
            };
            pull = {
              rebase = true;
            };
            core = {
              excludesfile = "~/.gitignore_global";
            };
            branch = {
              autosetupmerge = true;
            };
            rerere = {
              enabled = true;
            };
          };
        };
        programs.vscode = {
          enable = true;
          package = pkgs.vscodium;
          extensions = with pkgs.vscode-extensions; [
            bbenoist.nix
            kamadorueda.alejandra
            jdinhlife.gruvbox
            jnoortheen.nix-ide
            vscodevim.vim
            tekumara.typos-vscode
          ];
          userSettings = {
            "editor.minimap.enabled" = false;
            "editor.tabSize" = 2;
            "nix.enableLanguageServer" = true;
            "editor.fontSize" = 16;
            "nix.serverPath" = "nil";
          };
        };
        programs.alacritty = {
          enable = true;
          settings = {
            # env.TERM = "xterm-256color";
            window.dynamic_padding = true;
            window.decorations = "full";
            window.dynamic_title = true;
            scrolling.multiplier = 5;
            selection.save_to_clipboard = false;
            cursor.style.shape = "Underline";
            cursor.style.blinking = "on";
            cursor.unfocused_hollow = false;
            cursor.thickness = 0.10;
            window.padding = {
              x = 8;
              y = 8;
            };
          };
        };
        programs.helix = {
          enable = true;
          settings = {
            # theme = "";
            editor = {
              # line-number = "relative";
              lsp.display-messages = true;
            };
            # keys.normal = {
            #   space.space = "file_picker";
            #   space.w = ":w";
            #   space.q = ":q";
            #   esc = ["collapse_selection" "keep_primary_selection"];
            # };
          };
        };
        programs.kakoune = {
          enable = true;
          config = {
            colorScheme = "gruvbox-dark";
          };
        };
        programs.nixvim = {
          enable = true;
          colorschemes.gruvbox.enable = true;
        };
        programs.rofi = {
          package = pkgs.rofi-wayland;
          enable = true;
        };
        wayland.windowManager.hyprland = {
          enable = true;
        };
        services.flameshot = {
          enable = true;
        };

        dconf.settings = {
          "org/gnome/desktop/input-sources" = {
            show-all-sources = true;
            sources = [(lib.gvariant.mkTuple ["xkb" "pl"])];
            xkb-options = ["caps:escape"];
          };
        };
      };
    };

    programs.gnupg.agent = {
      enable = true;
    };

    # Open ports in the firewall.
    # networking.firewall.allowedTCPPorts = [ ... ];
    # networking.firewall.allowedUDPPorts = [ ... ];

    system.stateVersion = "24.05"; # Dont change
  }
