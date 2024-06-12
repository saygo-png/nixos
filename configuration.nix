#let
#        constants = {
#            username = "samsepi0l";
#            home = "/home/samsepi0l";
#            hostname = "nixos";
#            flake-path = "...";
#            system = "x86_64-linux";
#            version = "release-24.05";
#        };
#    in {
#    };
{
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    inputs.home-manager.nixosModules.default
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "default"; # Define your hostname.
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Warsaw";

  # Keyboard.
  i18n.defaultLocale = "en_US.UTF-8";
  services.xserver.xkb.layout = "pl";
  services.xserver.xkbOptions = "caps:swapescape";
  # services.xserver.xkb.options = "eurosign:e,caps:escape";
  console = {
    font = "Lat2-Terminus16";
    keyMap = lib.mkForce "pl"; # needs to be forced
    useXkbConfig = true; # use xkb.options in tty.
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
  };

  # Disable touchpad support (enabled default in most desktopManager).
  services.libinput.enable = false;

  system.activationScripts = {
    remaps = ''
      #!${pkgs.dash}/bin/dash
      # Decrease key repeat delay to 200ms and increase key repeat rate to 50 per second.
      ${pkgs.xorg.xset}/bin/xset r rate 140 50
    '';
  };

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

  programs.zsh.enable = true;

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
          VISUAL = "nvim";
          OPENER = "xdg-open";
          EDITOR = "nvim";
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
      };
      programs.alacritty = {
        enable = true;
        settings = {
          env.TERM = "xterm-256color";
          font = {
            size = 12;
          };
          scrolling.multiplier = 5;
          selection.save_to_clipboard = true;
        };
      };
      programs.zsh.enable = true;
      programs.kakoune = {
        enable = true;
      };
      programs.nixvim = {
        enable = true;
        colorschemes.gruvbox.enable = true;
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
