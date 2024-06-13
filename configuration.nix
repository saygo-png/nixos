let
  constants = {
    username = "samsepi0l";
    home = "/home/samsepi0l";
    hostname = "nixos";
    flake-path = "...";
    system = "x86_64-linux";
    version = "release-24.05";
    timezone = "Europe/Warsaw";
  };
in
  {
    pkgs,
    lib,
    config,
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

    networking.hostName = "default"; # Define your hostname.
    networking.networkmanager.enable = true;

    # Locale.
    i18n.defaultLocale = "en_US.UTF-8";
    time.timeZone = "${constants.timezone}";
    services.xserver.xkb = {
      layout = "pl";
      options = "caps:escape";
    };
    console = {
      useXkbConfig = true;
      font = "Lat2-Terminus16";
    };

    nix.settings.experimental-features = ["nix-command" "flakes"];

    # Define a user account. Don't forget to set a password with ‘passwd $USERNAME’.
    users.users.samsepi0l = {
      isNormalUser = true;
      extraGroups = ["wheel" "networkmanager"]; # Enable ‘sudo’ for the user.
      # shell = pkgs.zsh;
    };

    ############
    # Services #
    ############

    # Enable the X11 windowing system.
    services.xserver.enable = true;

    # Enable the GNOME Desktop Environment.
    services.xserver.displayManager.gdm.enable = true;
    services.xserver.desktopManager.gnome.enable = true;

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

    # Disable touchpad support (enabled default in most desktopManager).
    services.libinput.enable = false;

    environment.sessionVariables = {
      FLAKE = "/home/samsepi0l/nixos"; # For nix helper
    };

    environment.systemPackages = with pkgs; [
      # Nix stuff
      nh
      nvd
      nix-output-monitor

      wget
      dash
      git
      xdg-utils
      xdg-desktop-portal
      xdg-desktop-portal-gtk
      xdg-desktop-portal-hyprland
      libnotify
    ];

    stylix.enable = true;
    stylix.polarity = "dark";
    stylix.image = ./resources/wallpaper.png;
    stylix.cursor.package = pkgs.capitaine-cursors-themed;
    stylix.cursor.name = "Capitaine Cursors (Gruvbox)";
    stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/gruvbox-dark-medium.yaml";
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
            hello
            htop
            moar
            alejandra
            rofi
            librewolf
            nil
            btop
            iamb
            keepassxc
            rhythmbox
            inkscape
            krita
            cbonsai
            neofetch
            termdown
          ];

          sessionVariables = {
            # Default programs.
            PAGER = "moar";
            # Systemd is retarded and doesnt use normal pager variable :DDDDD
            SYSTEMD_PAGER = "moar";
            OPENER = "xdg-open";
            VISUAL = "nvim";
            EDITOR = "nvim";
            SUDO_EDITOR = "nvim";
            TERMINAL = "alacritty";
            TERMINAL_PROG = "alacritty";
            BROWSER = "librewolf";
            # Firefox hardware decode.
            MOZ_X11_EGL = 1;
            NO_AT_BRIDGE = 1;
            # Unreal engine .net cli tool turn off telemetry.
            DOTNET_CLI_TELEMETRY_OPTOUT = "true";
          };
        };
        # Development, internal.
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
        programs.zsh.enable = true;
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

        dconf.settings = {
          "org/gnome/desktop/input-sources" = {
            show-all-sources = true;
            sources = ["xkb"];
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
