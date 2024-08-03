{
  pkgs,
  lib,
  inputs,
  host,
  conUsername,
  conHome,
  conFlake-path,
  conAccentColor,
  conRefresh-rate,
  conScreen-width,
  conScreen-height,
  pkgs-unstable,
  ...
}: {
  imports = [
    inputs.stylix.nixosModules.stylix
    inputs.home-manager.nixosModules.default
  ];

  #######################
  # Essential or basic. #
  #######################

  # This is needed for building, by default its set to 10% of ram, but that might not be enough for low ram systems and u will get an "out of space" error when trying to build. This will still happen with this option, since you need the resize first to even apply this config. So put this line in the vanilla config, rebuild, and then build my config.
  services.logind.extraConfig = "RuntimeDirectorySize=4G";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Faster boot
  boot.initrd.systemd.network.wait-online.enable = false;
  networking.dhcpcd.wait = "background";

  networking.hostName = "${host}";
  networking.networkmanager.enable = true;

  # Optimization for ssds
  services.fstrim.enable = true;

  # DNS
  networking.nameservers = ["9.9.9.9" "149.112.112.112"];
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_CTYPE = "pl_PL.UTF-8";
      LC_NUMERIC = "pl_PL.UTF-8";
      LC_TIME = "pl_PL.UTF-8";
      LC_COLLATE = "pl_PL.UTF-8";
      LC_MONETARY = "pl_PL.UTF-8";
      LC_PAPER = "pl_PL.UTF-8";
      LC_NAME = "pl_PL.UTF-8";
      LC_ADDRESS = "pl_PL.UTF-8";
      LC_TELEPHONE = "pl_PL.UTF-8";
      LC_MEASUREMENT = "pl_PL.UTF-8";
      LC_IDENTIFICATION = "pl_PL.UTF-8";
    };
  };

  time.timeZone = "Europe/Warsaw";

  services.xserver.xkb = {
    extraLayouts = {
      plfi = {
        description = "plfi";
        languages = ["pol"];
        symbolsFile = ./resources/static/plfi;
      };
    };
    layout = "pl,plfi";
    options = "caps:escape,grp:sclk_toggle";
  };

  console = {
    useXkbConfig = true;
    font = "Lat2-Terminus16";
  };

  # Define a user account. Don't forget to set a password with ‘passwd $USERNAME’.
  users.users.${conUsername} = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
  };

  security.sudo.extraConfig = ''
    Defaults pwfeedback
    Defaults timestamp_timeout=-1
  '';

  ###################
  # NixOS programs. #
  ###################

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # Needed here and in home manager.
  programs.hyprland = {
    enable = true;
  };

  # File manager.
  programs.thunar.enable = true;

  # System packages.
  environment.systemPackages = with pkgs; [
    # Nix.
    nh # Nix helper
    nil # Nix LSP
    nix-output-monitor # Pretty nix build output
    alejandra # Nix formatter
    devenv # "Easy" dev envs

    # Other.
    wl-clipboard # Wayland xclip
    cliphist # Wayland clipboard manager
    nsxiv # Image viewer
    jq # Json parser, needed for "hyprland-next-visible-client.sh"
    nsxiv # Image viewer
    patool # Universal archiver
    libqalculate # Calculator
    conky # Hardware monitor
    vim # Text editor
    wget # Downloader
    eza # ls rewrite
    trashy # Cli trashcan
    udiskie # Auto mount
    dash # Lightweight shell
    git # Source control
    fzf # Fuzzy finder
    file # File identifier
    imagemagick_light # Image identifier
    ripgrep # Multithreaded grep
    gnumake # C compiling
    xdg-utils # Includes xdg-open
    gcc # C compiling
    fd # Find alternative
    libnotify # Notifications (notify-send)

    # For Hyprland
    qt5.qtwayland
    qt6.qtwayland
    libsForQt5.qt5ct
    libsForQt5.qt5.qtwayland
    kdePackages.qtwayland

    # These are filepickers and whatnot
    xdg-desktop-portal-gtk
    xdg-desktop-portal-hyprland
    hyprland-protocols

    # Shellscripts.
    (writeShellScriptBin "hyprland-next-visible-client.sh" (builtins.readFile ./resources/scripts/hyprland-next-visible-client.sh))

    (writeShellScriptBin "vmrss" (builtins.readFile ./resources/scripts/vmrss.sh))

    (writeShellScriptBin
      "update_krita.sh" # updates the flake krita nixos configuration files from current mutable krita config.

      ''
        set -o pipefail
        set -u
        shopt -s failglob
        KRITAHOME="${conHome}/.local/share/krita"
        KRITANIXHOME="${conFlake-path}/resources/krita"

        cp -vrf "$KRITAHOME/." "$KRITANIXHOME/krita-toplevel"
        cp -vf "$HOME/.config/kritarc" "$KRITANIXHOME/kritarc"
        cp -vf "$HOME/.config/kritadisplayrc" "$KRITANIXHOME/kritadisplayrc"

        # Dont include the cache
        rm -vf "$KRITANIXHOME/krita-toplevel/resourcecache.sqlite"
      '')

    (writeShellScriptBin
      "hwinfolist"
      ''
        echo "GPU"
        echo "nr amdgpu_top --gui"
        echo "General Tree"
        echo "snr lshw -json | nvim -c 'set filetype=json'"
      '')

    (writeShellScriptBin
      "clean-nix"
      ''
        nix-env --delete-generations 3d
        sudo nix-env --delete-generations 3d

        nix-collect-garbage --delete-older-than 3d
        sudo nix-collect-garbage --delete-older-than 3d

        nix-collect-garbage -d
        sudo nix-collect-garbage -d

        nix-store --gc
        sudo nix-store --gc

        nix store optimise
        sudo nix store optimise

        rm -rf ${conHome}/.local/state/home-manager
        rm -f ${conHome}/.local/state/nix/profiles/home-manager*
      '')

    (writeShellScriptBin
      "connection-tester"
      ''
        connected_to_internet() {
          test_urls="\
          https://www.google.com/ \
          https://www.duck.com/ \
          https://www.cloudflare.com/ \
          "
          processes="0"
          pids=""
          for test_url in $test_urls; do
            curl --silent --head "$test_url" > /dev/null &
            pids="$pids $!"
            processes=$(($processes + 1))
          done
          while [ $processes -gt 0 ]; do
            for pid in $pids; do
              if ! ps | grep "^[[:blank:]]*$pid[[:blank:]]" > /dev/null; then
                # Process no longer running
                processes=$(($processes - 1))
                pids=$(echo "$pids" | sed --regexp-extended "s/(^| )$pid($| )/ /g")

                if wait $pid; then
                  # Success! We have a connection to at least one public site, so the
                  # internet is up. Ignore other exit statuses.
                  kill -TERM $pids > /dev/null 2>&1 || true
                  wait $pids
                  return 0
                fi
              fi
            done
            wait -n $pids # Better than sleep, but not supported on all systems
          done
          return 1
        }
        if connected_to_internet; then
            echo "Connected to internet" && ${lib.getExe pkgs.libnotify} "Connected to internet"
          else
            echo "No internet connection"
        fi
      '')

    (writeShellScriptBin
      "pizzatimer"
      ''
        ${lib.getExe pkgs.termdown} 15m && \
        for i in {1..10}; do ${lib.getExe pkgs.libnotify} "Pizza is done."; done
      '')

    (writeShellScriptBin "hyprcorder.sh" ''
      set -e
      recproc=$(ps -A | grep wl-screenrec | awk '{print $1}')
      if [ $recproc ]; then
        kill -2 $recproc
        notify-send "Recording stopped."
        pushd ${conHome}/screencaptures/
        ${lib.getExe pkgs.ripdrag} $(ls -t | head -n1)
        popd
        exit
      else
        monitor=$(hyprctl activeworkspace -j | jq -r '.monitor')
        notify-send "Recording starting on $monitor."
        ${lib.getExe pkgs.wl-screenrec} --audio -b "1 MB" -o $monitor -f ${conHome}/screencaptures/$(date  +"%y.%m.%d-%H:%M").mp4
        fi
    '')
  ];

  ########################
  # Hardware or drivers. #
  ########################

  services.libinput.enable = true;
  services.libinput.mouse.accelProfile = "flat";

  # Most software has the HIP libraries hard-coded. You can work around it on NixOS by using:
  systemd.tmpfiles.rules = [
    "L+    /opt/rocm/hip   -    -    -     -    ${pkgs.rocmPackages.clr}"
  ];

  ##########
  # NixOs. #
  ##########

  nixpkgs.config.allowUnfree = false;
  nix.settings.experimental-features = ["nix-command" "flakes"];
  nix.settings.auto-optimise-store = true;
  nix.gc = {
    automatic = true;
    dates = "2day";
    options = "--delete-older-than 15d";
  };

  #############
  # Services. #
  #############

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Thumbnails for thunar
  services.tumbler.enable = true;

  # Might fix authorization agent issues
  services.gnome.gnome-keyring.enable = true;

  # Polkit (needed for window managers)
  security.polkit.enable = true;

  # NixOS is retarded and turns on lightdm by default.
  services.xserver.displayManager = lib.mkDefault {
    lightdm.enable = false;
    sx.enable = true;
  };
  services.displayManager.defaultSession = lib.mkDefault "none+awesome";

  # File synchronization.
  services.syncthing = {
    enable = true;
    dataDir = conHome;
    user = conUsername;
    openDefaultPorts = true;
    overrideDevices = true;
    overrideFolders = false;
    settings.options.relaysEnabled = false;
    settings.devices = {
      phone = {
        addresses = [
          "tcp://192.168.1.10:22000"
        ];
        id = "Z7AOC2O-CYXT6XV-Y67O5RB-VAXE2JT-JV36AMW-KWQ3U6Z-PVTINXB-IQ2UHQ7";
        autoAcceptFolders = true;
      };
      thinkpad = {
        addresses = [
          "tcp://192.168.1.13:22000 "
        ];
        id = "R3RAH4P-BEWWRO6-S5HYB2N-HZIHYCH-ERUDTE2-R2XLRAQ-CAZNG7U-S5BYYAF";
        autoAcceptFolders = true;
      };
    };
  };

  # Enable sound with low latency.
  hardware.pulseaudio.enable = false;
  # RealtimeKit service, which hands out realtime scheduling priority to user processes on demand. For example, the PulseAudio server uses this to acquire realtime priority.
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    wireplumber.enable = true;
    pulse.enable = true;
    alsa.enable = true;
    # Quirky low latency ig for gaming (it crunches)
    # alsa.support32Bit = true;
    # extraConfig.pipewire."92-low-latency" = {
    #   context.properties = {
    #     default.clock.rate = 48000;
    #     default.clock.quantum = 24;
    #     default.clock.min-quantum = 24;
    #     default.clock.max-quantum = 24;
    #   };
    # };
  };

  #########
  # Shell #
  #########

  programs.zsh.enable = true;

  # Declare zsh as an available shell.
  environment.shells = [pkgs.zsh];

  # Provides autocompletion for system programs for zsh.
  environment.pathsToLink = ["/share/zsh"];

  # Envvar, envars. User ones go into home manager.
  environment.sessionVariables = {
    FLAKE = "${conFlake-path}"; # For nix helper.
    GTK_USE_PORTAL = "1";

    # Transparent fzf.
    FZF_DEFAULT_OPTS = "--color=bg+:-1,gutter:-1,bg:-1";
  };

  ###########
  # Visuals #
  ###########

  # Fonts.
  fonts = {
    packages = with pkgs; [
      # Main font.
      courier-prime
      noto-fonts
      roboto
      jetbrains-mono
      noto-fonts-emoji
      noto-fonts-cjk-sans
      noto-fonts-cjk-serif
      (nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
    ];

    fontconfig = {
      enable = true;
      antialias = true;

      hinting = {
        enable = true;
        style = "full";
        autohint = false;
      };

      subpixel = {
        rgba = "none";
        lcdfilter = "default";
      };

      defaultFonts = {
        serif = ["Courier Prime" "Symbols Nerd Font"];
        sansSerif = ["Courier Prime" "Symbols Nerd Font"];
        monospace = ["Courier Prime" "Symbols Nerd Font"];
      };
    };
  };

  stylix.enable = true;
  stylix.autoEnable = true;
  stylix.polarity = "dark";
  stylix.targets.grub.useImage = true;
  stylix.image = ./resources/static/wallpaper.png;
  stylix.cursor.name = "Capitaine Cursors (Gruvbox)";
  stylix.cursor.package = pkgs.capitaine-cursors-themed;
  stylix.base16Scheme = {
    base00 = "282828"; # #282828 dark  ----
    base01 = "3c3836"; # #3c3836 dark  ---
    base02 = "504945"; # #504945 dark  --
    base03 = "665c54"; # #665c54 dark  -
    base04 = "bdae93"; # #bdae93 light +
    base05 = "d5c4a1"; # #d5c4a1 light ++
    base06 = "ebdbb2"; # #ebdbb2 light +++
    base07 = "fbf1c7"; # #fbf1c7 light ++++
    base08 = "fb4934"; # #fb4934 red
    base09 = "fe8019"; # #fe8019 orange
    base0A = "fabd2f"; # #fabd2f yellow
    base0B = "b8bb26"; # #b8bb26 green
    base0C = "8ec07c"; # #8ec07c cyan
    base0D = "83a598"; # #83a598 blue
    base0E = "d3869b"; # #d3869b purple
    base0F = "d65d0e"; # #d65d0e brown
  };

  stylix.fonts = {
    monospace = {
      name = "Courier Prime";
      package = pkgs.courier-prime;
    };
    sansSerif = {
      name = "Courier Prime";
      package = pkgs.courier-prime;
    };
    serif = {
      name = "Courier Prime";
      package = pkgs.courier-prime;
    };
    emoji = {
      name = "Symbols Nerd Font";
      package = pkgs.nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];};
    };
  };

  stylix.fonts.sizes = {
    popups = 13;
    desktop = 13;
    terminal = 13;
    applications = 12;
  };

  stylix.opacity = {
    popups = 0.8;
    desktop = 0.8;
    terminal = 0.8;
    applications = 0.5;
  };

  # Supposedly fixes some themeing/cursor issues might be useless.
  programs.dconf.enable = true;

  ################
  # HOME MANAGER #
  ################

  home-manager = {
    extraSpecialArgs = {inherit inputs pkgs-unstable;};
    backupFileExtension = "backup"; # h-m breaks without it.
    users.${conUsername} = {
      lib,
      config,
      formats,
      rasi,
      ...
    }: {
      imports = [
        inputs.nix-index-database.hmModules.nix-index
        inputs.nixvim.homeManagerModules.nixvim
      ];
      home = {
        username = "${conUsername}";
        homeDirectory = "${conHome}";
        stateVersion = "24.05"; # Dont change # CHANGE IT ON UPDATE NO BALLS

        shellAliases = {
          "neov" = "neovide --no-fork";
          "shutdown" = "poweroff";
          "ls" = "${lib.getExe pkgs.eza}";
          "la" = "${lib.getExe pkgs.eza} -a";
          "ll" = "${lib.getExe pkgs.eza} -l";
          "rt" = "${lib.getExe pkgs.trashy}";
          "pmem" = "vmrss"; # [p]rocess [mem]ory
          "qcalc" = "${lib.getExe pkgs.libqalculate}";
          "grep" = "${lib.getExe pkgs.gnugrep} --color=auto";
          "tree" = "${lib.getExe pkgs.eza} --group-directories-first --tree";
          "nhoffline" = "nh os switch ${conFlake-path} -- --option substitute false";
          "search" = "sudo find / -maxdepth 99999999 2>/dev/null | ${lib.getExe pkgs.fzf} -i -q $1";
          "listinstalledpackages" = "nix-store --query --requisites /run/current-system | cut -d- -f2- | sort -u";
          "record" = "${lib.getExe' pkgs.alsa-utils "arecord"} -t wav -r 48000 -c 1 -f S16_LE ${conHome}/screencaptures/recording.wav";
        };

        sessionVariables = {
          # Default programs.
          PAGER = lib.getExe pkgs.moar;
          # Systemd is retarded and doesnt use normal pager variable :DDDDD
          EDITOR = "nvim";
          VISUAL = "nvim";
          SUDO_EDITOR = "nvim";
          SYSTEMD_PAGER = lib.getExe pkgs.moar;
          TERMINAL = lib.getExe pkgs.alacritty;
          TERMINAL_PROG = lib.getExe pkgs.alacritty;
          BROWSER = lib.getExe pkgs-unstable.librewolf;
          OPENER = lib.getExe' pkgs.xdg-utils "xdg-open";
          # Firefox hardware decode.
          MOZ_X11_EGL = 1;
          NO_AT_BRIDGE = 1;
          # Unreal engine .net cli tool turn off telemetry.
          QT_QPA_PLATFORMTHEME = "qt5ct";
          DOTNET_CLI_TELEMETRY_OPTOUT = "true";
        };

        # Home packages, home manager packages, user packages
        packages = with pkgs; [
          # GUI.
          anki # Flashcards
          neovide # Neovim gui
          tauon # Music player
          foliate # Ebook reader
          rofi-wayland # App launcher
          keepassxc # Password manager
          qbittorrent # Torrent client

          python3

          # Command line.
          bc # Gnu calculator, needed for vmrss

          # Haskell
          haskell-language-server # Haskell LSP
          ghc # Haskell LSP

          moar # Pager
          termdown # Timer
          tldr # Man alternative
          htop # TUI task manager
          zoxide # Cd alternative
          hyprpicker # Color picker
          pulsemixer # Volume control
          ffmpeg # Video and magic editor
          swappy # Quick drawing on images

          # Unstable
          pkgs-unstable.krita # Painting
          pkgs-unstable.inkscape # Painting
          pkgs-unstable.librewolf # Browser
          # DO THIS ONE LIBREWOLF GETS A HOME MANAGER MODULE TO MOVE .mozzila INTO CONFIG HOME
          # programs = {
          #   # use firefox dev edition
          #   firefox = rec {
          #     enable = true;
          #     package = pkgs.firefox-devedition-bin.overrideAttrs (o: {
          #       # launch firefox with user profile
          #       buildCommand =
          #         o.buildCommand
          #         + ''
          #           wrapProgram "$executablePath" \
          #             --set 'HOME' '${config.xdg.configHome}' \
          #             --append-flags "--name firefox -P ${user}"
          #         '';
          #     });
          #
          #     vendorPath = ".config/.mozilla";
          #     configPath = "${vendorPath}/firefox";
          #   };
          # };
          pkgs-unstable.gomuks # TUI matrix client
        ];

        # Binary blobs.
        sessionPath = ["${conHome}/bin"]; # Add ~/bin to path.
        file."bin/tmux-mem-cpp".source = ./resources/static/tmux-mem-cpp;
        file."bin/ow".source = ./resources/scripts/ow.py;

        # This allows for semi-declarative configuration.
        # However it makes your lag when rebuilding.
        activation.configure-krita = lib.hm.dag.entryAfter ["writeBoundary"] ''
          mkdir -p "${config.xdg.configHome}"
          mkdir -p "${conHome}/.local/share/krita"
          run chmod -R $VERBOSE_ARG u+w,g+w "${conHome}/.local/share/krita"
          run cp -rf $VERBOSE_ARG "${builtins.toPath ./resources/krita/kritarc}" "${config.xdg.configHome}/kritarc"
          run cp -rf $VERBOSE_ARG "${builtins.toPath ./resources/krita/kritadisplayrc}" "${config.xdg.configHome}/kritadisplayrc"
          run cp -rf $VERBOSE_ARG "${builtins.toPath ./resources/krita/krita-toplevel}"/. "${conHome}/.local/share/krita"
          run chmod -R $VERBOSE_ARG u+w,g+w "${conHome}/.local/share/krita"
        '';
      };

      # Needed for transparency.
      stylix.targets.fzf.enable = false;

      # Unable to set wallpaper bug fix
      stylix.targets.kde.enable = false;

      # More visuals.
      gtk = {
        enable = true;
        iconTheme = {
          name = "Gruvbox-Plus-Dark";
          package = pkgs.gruvbox-plus-icons;
        };
      };

      qt = {
        enable = true;
        platformTheme.name = "qtct";
        style.package = with pkgs; [
          adwaita-qt
          adwaita-qt6
        ];
      };

      # Development, internal.
      programs.lazygit = {
        enable = true;
        settings = {
          gui.border = "single";
        };
      };
      programs.bash.enable = true;
      programs.zoxide.enable = true;
      programs.nix-index.enable = true;
      programs.home-manager.enable = true;
      programs.git-credential-oauth.enable = true;

      services.dunst = {
        enable = true;
        settings = {
          global = {
            width = 300;
            height = 300;
            offset = "30x50";
            origin = "top-center";
          };
        };
      };

      programs.mpv = {
        enable = true;
        bindings = {
          l = "seek 20";
          h = "seek -20";
          "]" = "add speed 0.1";
          "[" = "add speed -0.1";
          j = "seek -4";
          k = "seek 4";
          K = "cycle sub";
          J = "cycle sub down";
          w = "add sub-pos -10"; # move subtitles up
          W = "add sub-pos -1"; # move subtitles up
          e = "add sub-pos +10"; # move subtitles down
          E = "add sub-pos +1"; # move subtitles down
          "ALT+=" = "add sub-scale +0.1";
          "ALT+-" = "add sub-scale -0.1";
        };

        config = {
          speed = 1;
          hwdec = true;
          sub-pos = 90;
          keep-open = true;
          sub-auto = "all";
          sub-font-size = 40;
          sub-border-size = 2;
          sub-shadow-offset = 2;
          sub-visibility = "yes";
          sub-ass-line-spacing = 1;
          sub-ass-hinting = "normal";
          sub-ass-override = "force";
          save-position-on-quit = true;
          sub-auto-exts = "srt,ass,txt";
          ytdl-format = "bestvideo+bestaudio/best";
          slang = "fin,fi,fi-fi,eng,en,en-en,en-orig";
          sub-font = "${config.stylix.fonts.serif.name}";
          sub-ass-force-style = "${config.stylix.fonts.serif.name}";
          sub-color = "${config.lib.stylix.colors.withHashtag.base07}";
          sub-shadow-color = "${config.lib.stylix.colors.withHashtag.base00}";
        };
        scripts = [
          pkgs.mpvScripts.uosc
          pkgs.mpvScripts.acompressor
        ];
      };

      programs.yazi = {
        enable = true;
        enableZshIntegration = true;
        settings = {
          manager = {
            show_hidden = true;
            sort_dir_first = true;
          };
        };
      };

      programs.fzf = {
        enable = true;
      };

      programs.zsh = {
        enable = true;
        enableCompletion = false;
        defaultKeymap = "viins";
        autosuggestion.enable = true;
        syntaxHighlighting.enable = true;
        historySubstringSearch.enable = true;
        syntaxHighlighting.highlighters = [
          "brackets"
        ];
        initExtra = builtins.readFile ./resources/zsh-extraConfig;

        history.size = 50;
        history.save = 50;
      };

      programs.btop = {
        enable = true;
        settings = {
          theme_background = false;
          vim_keys = true;
          rounded_corners = false;
        };
      };

      programs.direnv = {
        enable = true;
        nix-direnv.enable = true;
      };

      programs.fastfetch = {
        enable = true;
        settings = {
          modules = [
            "title"
            "separator"
            "os"
            "host"
            "kernel"
            "uptime"
            "packages"
            "shell"
            "display"
            "de"
            "wm"
            "wmtheme"
            "theme"
            "icons"
            "font"
            "cursor"
            "terminal"
            "terminalfont"
            "cpu"
            "gpu"
            "memory"
            "swap"
            "disk"
            "battery"
            "poweradapter"
            "locale"
            "break"
            "colors"
          ];
        };
      };

      programs.git = {
        enable = true;
        package = pkgs.gitAndTools.gitFull;
        userName = "saygo-png";
        userEmail = "saygo.mail@proton.me";
        aliases = {
          undo = "reset HEAD~1 --mixed";
          amend = "commit -a --amend";
          aa = "add -A"; # [A]dd [A]ll
        };
        extraConfig = {
          # credential = {
          #   helper = lib.mkForce "cache --timeout 21600"; # six hours
          # };
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

      programs.foot = {
        enable = true;
        settings = {
          main = {
            term = "foot";
            pad = "6x6center";
          };
          mouse = {
            hide-when-typing = "yes";
          };
          cursor = {
            blink = "yes";
          };
          scrollback = {
            lines = 1000;
          };
        };
      };

      programs.alacritty = {
        enable = true;
        settings = {
          scrolling.multiplier = 5;
          window.dynamic_title = true;
          cursor.style.blinking = "on";
          window.dynamic_padding = true;
          cursor.unfocused_hollow = false;
          cursor.style.shape = "Underline";
          selection.save_to_clipboard = false;
          scrolling.history = 1000;
          window.padding = {
            x = 8;
            y = 8;
          };
          font.offset.y = 3; # Line spacing
          keyboard.bindings = [
            {
              key = "Escape";
              mods = "Shift";
              action = "ToggleViMode";
            }
          ];
        };
      };

      programs.nixvim = {
        enable = true;
        extraPackages = with pkgs; [
          typos-lsp
          vale # Linter
          jq # Json formatter
          vim-language-server
          deadnix # Nix linter
          ruff # Python linter
          nodePackages.jsonlint
          hlint # Haskell linter
          stylua # Lua formatter
          marksman # Markdown LSP
          shfmt # Shell formatter
          yapf # Python formatter
          black # Python formatter
          hadolint # Docker linter
          rust-analyzer # Rust LSP
          shellcheck # Bash linter
          mypy # Python type checker
          sumneko-lua-language-server
          isort # Python import sorter
          nodePackages.bash-language-server
          python312Packages.python-lsp-server
          stylish-haskell # Haskell formatter
          haskell-language-server # Haskell lsp
          python312Packages.flake8 # Pylsp plugin
          python312Packages.mccabe # Flake8 plugin
          python312Packages.pyflakes # Python linter
          haskellPackages.fourmolu # Haskell formatter
          luajitPackages.jsregexp # Needed for luasnip
          python312Packages.jedi # Autocomplete plugin
          python312Packages.pycodestyle # Python complainer
          python312Packages.pyls-isort # Python import sort
          python312Packages.pylsp-rope # Refactoring plugin
          python312Packages.pylsp-mypy # Static checker plugin
        ];

        highlightOverride = {
          # hi noCursor blend=100 cterm=strikethrough
          noCursor.blend = 100;
          statusline.bg = "NONE";
          ModeMsg.fg = "#${conAccentColor}";
          MsgArea.fg = "#${conAccentColor}";
          statusline.fg = "#${conAccentColor}";
          FloatBorder.fg = "#${conAccentColor}";
          CursorLineNr.fg = "#${conAccentColor}";
          CursorLineNr.bg = "${config.lib.stylix.colors.withHashtag.base01}"; # Gray numberline
          MiniIndentscopeSymbol.fg = "${config.lib.stylix.colors.withHashtag.base01}"; # Gray indentline
        };

        opts = {
          # Indents.
          tabstop = 2;
          shiftwidth = 2;
          softtabstop = 2;
          expandtab = true;
          autoindent = true;
          breakindent = true; # Indent when wrapping

          # Wrapping.
          wrap = false;

          # Center it all.
          scrolloff = 999;
          sidescrolloff = 999;

          # Delay on switching to normal mode.
          ttimeoutlen = 0;

          # g in substitute implicit
          gdefault = true;

          # Incremental search.
          incsearch = true;
          updatetime = 100;

          # Relative numberline on the left.
          number = true;
          relativenumber = true;

          # Color current line number.
          cursorline = true;
          cursorlineopt = "number";

          # Smartcase search and ripgrep.
          smartcase = true;
          ignorecase = true;
          grepprg = "rg --vimgrep";
          grepformat = "%f:%l:%c:%m";

          # Folds.
          foldmethod = "marker";
          foldlevel = 99; # Ufo provider needs a large value, feel free to decrease the value
          foldlevelstart = 99;
          foldenable = true;

          # More space.
          cmdheight = 0;

          # Puts error messages on the number line.
          signcolumn = "number";

          # Show some whitespace.
          list = true;
          listchars = "tab:▸ ,trail:·,nbsp:␣";

          # Better completion.
          completeopt = ["menuone" "noselect" "noinsert"];

          # Use conform-nvim for gq formatting. ('formatexpr' is set to vim.lsp.formatexpr(),
          # so you can format lines via gq if the language server supports it).
          formatexpr = "v:lua.require'conform'.formatexpr()";

          # (https://neovim.io/doc/user/options.html#'laststatus')
          laststatus = 3;
        };
        globals = {
          mapleader = " ";

          rainbow_active = 1;

          gruvbox_material_foreground = "original";
          gruvbox_material_enable_bold = 0;
          gruvbox_material_transparent_background = 2;

          # Neovide neovim gui client.
          neovide_transparency = config.stylix.opacity.terminal;
          neovide_transparency_point = 0; # config.stylix.opacity.terminal;
          neovide_background_color = "${config.lib.stylix.colors.withHashtag.base00}";
          neovide_padding_top = 8;
          neovide_padding_bottom = 0;
          neovide_padding_right = 4;
          neovide_padding_left = 4;
          neovide_floating_blur_amount_x = 20.0;
          neovide_floating_blur_amount_y = 20.0;
          neovide_hide_mouse_when_typing = true;
          neovide_refresh_rate = conRefresh-rate;
          neovide_cursor_vfx_mode = "ripple";
          neovide_cursor_animation_length = 0.08;
          neovide_cursor_smooth_blink = true;
          neovide_floating_shadow = false;
          neovide_cursor_animate_command_line = true;
          neovide_cursor_animate_in_insert_mode = true;
        };

        extraFiles = {
          "ftplugin/json.vim" = ''
            setlocal foldmethod=manual
          '';
        };

        extraConfigVim = builtins.readFile ./resources/nvim-extraConfig.vim;
        extraConfigLuaPre = ''
          -- Hide deprecation warnings
          local notify = vim.notify
          vim.notify = function(msg, ...)
            if msg:match("has been deprecated") then
              return
            end
            notify(msg, ...)
          end
        '';
        extraConfigLuaPost = ''
          -- Makes treesitter work with rainbow plugin
          vim.api.nvim_set_hl(0, "@constructor", { link = "" })
          vim.api.nvim_set_hl(0, "@constructor.lua", { link = "" })
          vim.api.nvim_set_hl(0, "@punctuation.bracket", { link = "" })
          vim.api.nvim_set_hl(0, "@punctuation.special", { link = "" })
          vim.api.nvim_set_hl(0, "@punctuation.delimiter", { link = "" })
          vim.api.nvim_set_hl(0, "@variable.parameter.haskell", { link = "" })
        '';

        extraConfigLua = ''
          if vim.g.neovide then
            vim.cmd[[colorscheme gruvbox-material]]
            vim.o.background = "dark"
            vim.o.guifont = "JetBrains Mono:h13:#e-antialias:#h-slight"
            vim.cmd [[ hi Normal guibg=${config.lib.stylix.colors.withHashtag.base00} ]]
          end

          vim.keymap.set("n", "<leader>r", function()
            -- when rename opens the prompt, this autocommand will trigger
            -- it will "press" CTRL-F to enter the command-line window `:h cmdwin`
            -- in this window I can use normal mode keybindings
            local cmdId
            cmdId = vim.api.nvim_create_autocmd({ "CmdlineEnter" }, {
              callback = function()
                local key = vim.api.nvim_replace_termcodes("<C-f>", true, false, true)
                vim.api.nvim_feedkeys(key, "c", false)
                vim.api.nvim_feedkeys("0", "n", false)
                -- autocmd was triggered and so we can remove the ID and return true to delete the autocmd
                cmdId = nil
                return true
              end,
            })
            vim.lsp.buf.rename()
            -- if LPS couldn't trigger rename on the symbol, clear the autocmd
            vim.defer_fn(function()
              -- the cmdId is not nil only if the LSP failed to rename
              if cmdId then
                vim.api.nvim_del_autocmd(cmdId)
              end
            end, 500)
          end)

          -- Transparent hover
          vim.api.nvim_set_hl(0, 'NormalFloat', { link = 'Normal', })

          require("cutlass").setup({
            cut_key = "m",
            override_del = true,
            exclude = { "ns", "nS" }, -- Motion plugins rebind this
          })

          -- Make lsp popups pretty.
          local border = {
            { '┌', 'FloatBorder' },
            { '─', 'FloatBorder' },
            { '┐', 'FloatBorder' },
            { '│', 'FloatBorder' },
            { '┘', 'FloatBorder' },
            { '─', 'FloatBorder' },
            { '└', 'FloatBorder' },
            { '│', 'FloatBorder' },
          }

          local _border = "single"
          require('lspconfig.ui.windows').default_options = { border = _border }
          vim.lsp.handlers["textDocument/hover"] = vim.lsp.with( vim.lsp.handlers.hover, { border = _border })
          vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with( vim.lsp.handlers.signature_help, { border = _border })

          vim.diagnostic.config({
          underline = false,
          update_in_insert = false,
          virtual_text = false,
          signs = true,
            float = {
              win_options = {
                winblend = 100
              },
              border = border,
              format = function(diagnostic)
                return string.format(
                  "%s (%s) [%s]",
                  diagnostic.message,
                  diagnostic.source,
                  diagnostic.code or diagnostic.user_data.lsp.code
                )
              end,
            },
          })

          -- Turn off cmp in comments.
          enabled = function()
             local in_prompt = vim.api.nvim_buf_get_option(0, 'buftype') == 'prompt'
             if in_prompt then  -- this will disable cmp in the Telescope window (taken from the default config)
               return false
             end
             local context = require("cmp.config.context")
             return not(context.in_treesitter_capture("comment") == true or context.in_syntax_group("Comment"))
          end
        '';
        package = pkgs.neovim-unwrapped;
        clipboard.register = "unnamedplus";

        colorschemes.base16.enable = lib.mkForce false;
        colorscheme = "gruvbox-material";

        extraPlugins = [
          (pkgs.vimUtils.buildVimPlugin {
            name = "cutlass.nvim";
            src = inputs.nvim-plugin-cutlass;
          })
          (pkgs.vimUtils.buildVimPlugin {
            name = "vim-visual-multi";
            src = inputs.nvim-plugin-vim-visual-multi;
          })
          (pkgs.vimUtils.buildVimPlugin {
            name = "rainbow";
            src = inputs.nvim-plugin-rainbow;
          })
          pkgs.vimPlugins.gruvbox-material
        ];

        plugins = {
          nix.enable = true;
          flash.enable = true;
          comment.enable = true;
          nvim-ufo.enable = true;
          surround.enable = true;
          friendly-snippets.enable = true;

          spider = {
            enable = true;
            skipInsignificantPunctuation = false;
            keymaps.motions = {
              b = "b";
              e = "e";
              ge = "ge";
              w = "w";
            };
          };

          harpoon = {
            enable = true;
            markBranch = true;
            enableTelescope = true;
            keymaps = {
              addFile = "<leader>ha";
              navFile = {
                "1" = "<C-j>";
                "2" = "<C-k>";
                "3" = "<C-l>";
                "4" = "<C-m>";
              };
              navNext = "<leader>hn";
              navPrev = "<leader>hp";
              toggleQuickMenu = "<leader>hm";
              cmdToggleQuickMenu = "<leader>hcm";
            };
          };

          nvim-colorizer = {
            enable = true;
            fileTypes = let
              css = {css = true;};
            in [
              "*"
              ({language = "css";} // css)
              ({language = "less";} // css)
              ({language = "sass";} // css)
              ({language = "scss";} // css)
              ({language = "stylus";} // css)
            ];
            bufTypes = [
              "*"
              "!prompt"
              "!popup"
            ];
            userDefaultOptions = {
              names = false;
            };
          };

          telescope = {
            enable = true;
            extensions.fzf-native.enable = true;
            keymaps = {
              "<leader>tb" = {
                action = "current_buffer_fuzzy_find ";
                options = {
                  desc = "[T]elescope [B]uffr search";
                };
              };
              "<leader>tg" = {
                action = "live_grep";
                options.desc = "[T]elescope [G]rep";
              };
            };
          };

          lspsaga = {
            enable = true;
            lightbulb = {
              enable = false;
              sign = false;
              virtualText = false;
            };
          };

          fidget = {
            enable = true;
            progress = {
              ignore = ["hls"]; # Hls keeps a popup on and its annoying
              ignoreDoneAlready = true;
              suppressOnInsert = true;
              pollRate = 1;
              display = {
                doneIcon = "ok"; # Icon shown when all LSP progress tasks are complete
              };
            };
            notification = {
              window = {
                normalHl = "Comment";
                winblend = 0;
                border = "single"; # none, single, double, rounded, solid, shadow
              };
            };
          };

          mini = {
            enable = true;
            modules = {
              indentscope = {
                draw = {
                  delay = 0;
                  priority = 2;
                };
                symbol = "│";
                options = {
                  border = "top";
                  indent_at_cursor = true;
                  try_as_border = true;
                };
              };
              align = {};
            };
          };

          trouble = {
            enable = true;
            settings = {
              auto_close = true;
            };
          };

          treesitter = {
            enable = true;
            indent = true;
            nixvimInjections = true;
            nixGrammars = true; # Install grammars with Nix
            ensureInstalled = ["all"];
            incrementalSelection = {
              enable = true;
              keymaps = {
                initSelection = "gnn";
                nodeIncremental = "grn";
                scopeIncremental = "grc";
                nodeDecremental = "grm";
              };
            };
            moduleConfig = {
              highlight = {
                enable = true;
              };
            };
          };

          lsp = {
            enable = true;
            # Disable highlights from LSP, breaks rainbow
            onAttach = ''
              client.server_capabilities.semanticTokensProvider = nil
            '';
            servers = {
              # Nix.
              nil-ls.enable = true;

              # Python.
              pylsp.enable = true;
              ruff.enable = true;

              # Bash.
              bashls.enable = true;

              # Typos.
              typos-lsp = {
                enable = true;
                extraOptions.init_options.diagnosticSeverity = "Hint";
              };

              # Lua.
              lua-ls.enable = true;

              # Haskell.
              hls.enable = true;

              # Rust.
              rust-analyzer = {
                enable = true;
                installCargo = false;
                installRustc = false;
              };

              # Filesystem.
              fsautocomplete.enable = true;
            };
            keymaps.lspBuf = {
              "gd" = "definition";
              "gD" = "references";
              "gi" = "implementation";
              "K" = "hover";
            };
          };

          gitsigns = {
            enable = true;
            settings = {
              current_line_blame = false;
              signcolumn = false;
            };
          };

          multicursors = {
            enable = true;
          };

          conform-nvim = {
            enable = true;
            extraOptions = {
              lsp_fallback = true;
            };
            formattersByFt = {
              # Conform will run multiple formatters sequentially.
              python = ["isort" "yapf"];
              haskell = ["fourmolu" "stylish-haskell"];
              nix = ["alejandra"];
              lua = ["stylua"];
              json = ["jq"];
              sh = ["shfmt"];
              # Use the "*" filetype to run formatters on all filetypes.
              "*" = ["trim_whitespace"];
            };
          };

          lint = {
            enable = true;
            lintersByFt = {
              text = ["vale"];
              json = ["jsonlint"];
              bash = ["shellcheck"];
              shell = ["shellcheck"];
              haskell = ["hlint"];
              markdown = ["vale"];
              python = ["ruff"];
              rst = ["vale"];
              clojure = ["clj-kondo"];
              dockerfile = ["hadolint"];
              nix = ["nix" "deadnix"];
            };
          };

          which-key = {
            enable = true;
            ignoreMissing = false;
            registrations = {
              "gd" = "[g]o to [d]efinition";
              "gD" = "[g]o to uses";
              "gi" = "[g]o to [i]mplementation";
              "K" = "[H]over info";
              "<Leader>rn" = "[r]e[n]ame";
              "<Leader>t" = "+[t]elescope";
              "<Leader>h" = "+[h]arpoon";
              "<leader>hh" = "[h]arpoon [a]dd file";
              "<leader>hm" = "[h]arpoon [m]enu";
              "<leader>hcm" = "[h]arpoon [c]ommand [m]enu";
              "<leader>hn" = "[h]arpoon [n]ext";
              "<leader>hp" = "[h]arpoon [p]revious";
              "<C-j>" = "harpoon file 1";
              "<C-k>" = "harpoon file 2";
              "<C-l>" = "harpoon file 3";
              "<C-m>" = "harpoon file 4";
            };
            plugins = {
              presets = {
                # Needs to be false for indent keybindings
                operators = false; #adds help for operators like d, y, ...";
              };
            };
          };

          oil = {
            enable = true;
            settings.defaultFileExplorer = true;
          };

          luasnip = {
            enable = true;
            extraConfig = {
              enable_autosnippets = true;
              store_selection_keys = "<Tab>";
            };
            fromVscode = [
              {
                lazyLoad = true;
              }
            ];
          };

          cmp-nvim-lsp.enable = true;
          cmp-nvim-lsp-signature-help.enable = true;
          cmp = {
            enable = true;
            autoEnableSources = true;

            settings = {
              autocomplete = true;
              performance = {
                debounce = 60;
                fetchingTimeout = 200;
                maxViewEntries = 30;
              };
              snippet.expand = ''
                function(args)
                  require('luasnip').lsp_expand(args.body)
                end
              '';
              sources = [
                {name = "nvim_lsp";}
                {name = "nvim_lua";}
                {name = "luasnip";}
                {name = "path";}
                {name = "treesitter";}
                {name = "nvim_lsp_signature_help";}
                {name = "nvim_lsp_document_symbol";}
              ];
              mapping = {
                "<Tab>" = "cmp.mapping(cmp.mapping.select_next_item(), {'i', 's'})";
                "<S-Tab>" = "cmp.mapping(cmp.mapping.select_prev_item(), {'i', 's'})";
                "<C-j>" = "cmp.mapping.select_next_item()";
                "<C-k>" = "cmp.mapping.select_prev_item()";
                "<C-e>" = "cmp.mapping.abort()";
                "<C-b>" = "cmp.mapping.scroll_docs(-4)";
                "<C-f>" = "cmp.mapping.scroll_docs(4)";
                "<C-Space>" = "cmp.mapping.complete()";
                "<CR>" = "cmp.mapping.confirm({ select = false })";
                "<S-CR>" = "cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true })";
              };

              window = {
                completion.scrollbar = true;
                documentation.border = "single";
              };
            };
          };
        };

        keymaps = [
          # Basic.
          {
            mode = "n";
            action = "Gzz";
            key = "G";
            options.desc = "Center bottom";
          }
          {
            mode = "n";
            action = "ggzz";
            key = "gg";
            options.desc = "Center top";
          }
          {
            mode = "n";
            action = "gj";
            key = "j";
            options.desc = "Move down through wrapped line";
          }
          {
            mode = "n";
            action = "gk";
            key = "k";
            options.desc = "Move up through wrapped line";
          }
          {
            mode = "n";
            action = ":";
            key = ";";
            options.desc = "Command mode with or without shift";
          }
          {
            mode = "n";
            action = ":";
            key = ";";
            options.desc = "Command mode with or without shift";
          }
          {
            key = "<leader>th";
            action = ":Telescope harpoon marks<cr>";
            options = {
              silent = true;
              desc = "[t]elescope [h]arpoon Marks";
            };
          }
          {
            key = "s";
            action.__raw = ''require("flash").remote'';
            options.desc = "Flash";
          }
          {
            key = "S";
            action.__raw = ''require("flash").treesitter'';
            options.desc = "Flash treesitter";
          }
          {
            mode = "n";
            action = "<lt><lt>";
            key = "<lt>";
            options.desc = "Indent less";
            options.silent = true;
          }
          {
            mode = "n";
            action = ">>";
            key = ">";
            options.desc = "Indent more";
            options.silent = true;
          }
          {
            mode = "v";
            action = ":normal .<CR>";
            key = ".";
            options.desc = "Dot commands over visual blocks";
          }

          {
            mode = ["n"];
            action = ''"+p'';
            key = "<C-V>";
            options.desc = "Proper paste";
          }
          {
            mode = ["i" "c"];
            action = ''<C-r>+'';
            key = "<C-V>";
            options.desc = "Proper paste";
          }

          # Plugin garbage.
          {
            mode = "n";
            key = "<Leader>c";
            action = "<cmd>lua require('conform').format({ timeout_ms = 500 })<CR>";
            options.desc = "[c]onform";
          }

          {
            mode = "n";
            key = "gm";
            action = "m";
            options.desc = "Set mark";
          }
          {
            action = "<cmd>Oil .<CR>";
            key = "<Leader>f";
            options.desc = "Open [f]ile explorer";
          }
          {
            action = "<cmd>Telescope find_files<CR>";
            key = "<leader>tf";
            options.desc = "Telescope find [f]iles";
          }
          {
            action = "<cmd>Telescope buffers<CR>";
            key = "<Leader>to";
            options.desc = "Pick [o]pen buffers";
          }
          {
            action = "<cmd>Telescope live_grep<CR>";
            key = "<Leader>tt";
            options.desc = "Find [t]ext in project";
          }
          {
            action = "<cmd>lua vim.lsp.buf.code_action()<CR>";
            key = "<Leader>la";
            options.desc = "Code [a]ctions";
          }
          {
            action = "<cmd>lua vim.diagnostic.open_float()<CR>";
            key = "<Leader>e";
            options.desc = "Open diagnostic";
          }
          {
            action = '':!awk '{ print length(), $0 | "sort -n | cut -d\\  -f2-" }'<CR><CR>'';
            key = "<Leader>s";
            options.silent = true;
            options.desc = "[s]ort lines by length";
          }
          {
            mode = "n";
            action = "<cmd>nohlsearch<CR>";
            key = "<Esc>";
          }
        ];

        autoCmd = [
          {
            event = ["BufReadPost"];
            pattern = ["*"];
            command = "normal!'\"";
            desc = "Open at last location";
          }
          {
            event = ["BufEnter"];
            pattern = ["*"];
            command = "setlocal formatoptions-=c formatoptions-=r formatoptions-=o";
            desc = "Dont insert comments on newline";
          }
        ];
      };

      programs.rofi = {
        package = pkgs.rofi-wayland;
        enable = true;
        extraConfig = {
          modi = "window,run,drun";
          font = "${config.stylix.fonts.monospace.name} 14";
          padding = 10;
          fixed-num-lines = true;
          show-icons = false;
          terminal = "alacritty";
          run-command = "{cmd}";
          drun-show-actions = false;
          disable-history = false;
          matching = "fuzzy";
          sort = true;
          scrollbar = false;
          sorting-method = "fzf";
          auto-select = false;
          separator-style = "dash";
          window-format = "{w} {c}  {t}";
          show-match = true;

          # Bindings, I use empty strings to remove bind conflicts.
          kb-remove-to-eol = "";
          kb-row-up = "Control+k";

          kb-accept-entry = "Return";
          kb-row-down = "Control+j";

          kb-mode-complete = "";
          kb-mode-next = "Control+l";

          kb-remove-char-forward = "";
          kb-clear-line = "Control+d";

          kb-remove-char-back = "BackSpace";
          kb-mode-previous = "Control+h";

          kb-cancel = "Escape,Control+q";
        };
        theme = let
          inherit (config.lib.formats.rasi) mkLiteral;
        in {
          "*" = {
            highlight = "bold";
            border-color = lib.mkForce (mkLiteral "#7d8618");
            gruvbox-dark-fg0 = lib.mkForce (mkLiteral "#fbf1c7");
            gruvbox-dark-fg1 = lib.mkForce (mkLiteral "#ebdbb2");
            gruvbox-dark-gray = lib.mkForce (mkLiteral "#bdae93");
            background-color = lib.mkForce (mkLiteral "@background");
            background = lib.mkForce (mkLiteral "@gruvbox-dark-bg0");
            foreground = lib.mkForce (mkLiteral "@gruvbox-dark-fg1");
            separatorcolor = lib.mkForce (mkLiteral "@border-color");
            active-foreground = lib.mkForce (mkLiteral "@foreground");
            gruvbox-dark-red-dark = lib.mkForce (mkLiteral "#fe8019");
            normal-foreground = lib.mkForce (mkLiteral "@foreground");
            gruvbox-dark-red-light = lib.mkForce (mkLiteral "#fb4934");
            scrollbar-handle = lib.mkForce (mkLiteral "@border-color");
            gruvbox-dark-yellow-dark = lib.mkForce (mkLiteral "#fabd2f");
            gruvbox-dark-yellow-light = lib.mkForce (mkLiteral "#8ec07c");
            urgent-foreground = lib.mkForce (mkLiteral "@gruvbox-dark-fg1");
            alternate-normal-foreground = lib.mkForce (mkLiteral "@foreground");
            gruvbox-dark-bg0 = lib.mkForce (mkLiteral "rgba (40, 40, 40, 100%)");
            urgent-background = lib.mkForce (mkLiteral "@gruvbox-dark-red-dark");
            normal-background = lib.mkForce (mkLiteral "rgba (40, 40, 40, 100%)");
            gruvbox-dark-bg3 = lib.mkForce (mkLiteral "rgba (125, 134, 24, 100%)");
            active-background = lib.mkForce (mkLiteral "@gruvbox-dark-yellow-dark");
            selected-normal-background = lib.mkForce (mkLiteral "@gruvbox-dark-bg3");
            selected-normal-foreground = lib.mkForce (mkLiteral "@gruvbox-dark-fg0");
            alternate-urgent-foreground = lib.mkForce (mkLiteral "@gruvbox-dark-fg1");
            gruvbox-dark-bg0-soft = lib.mkForce (mkLiteral "rgba (40, 40, 40, 100%)");
            selected-active-foreground = lib.mkForce (mkLiteral "@active-foreground");
            selected-urgent-foreground = lib.mkForce (mkLiteral "@urgent-foreground");
            alternate-active-background = lib.mkForce (mkLiteral "@active-background");
            alternate-active-foreground = lib.mkForce (mkLiteral "@active-foreground");
            alternate-urgent-background = lib.mkForce (mkLiteral "@urgent-background");
            selected-urgent-background = lib.mkForce (mkLiteral "@gruvbox-dark-red-light");
            alternate-normal-background = lib.mkForce (mkLiteral "rgba (40, 40, 40, 100%)");
            selected-active-background = lib.mkForce (mkLiteral "@gruvbox-dark-yellow-light");
          };
          "window" = {
            border = 1;
            padding = 5;
            background-color = lib.mkForce (mkLiteral "@background");
          };
          "mainbox" = {
            border = 0;
            padding = 0;
          };
          "message" = {
            padding = mkLiteral "1px";
            border = mkLiteral "2px 0 0";
            border-color = lib.mkForce (mkLiteral "rgba (40, 40, 40, 100%)");
          };
          "textbox" = {
            highlight = mkLiteral "@highlight";
            text-color = lib.mkForce (mkLiteral "@foreground");
          };
          "listview" = {
            spacing = mkLiteral "0px";
            padding = mkLiteral "0px 0 0";
            border = mkLiteral "0px solid 0 0";
            border-color = lib.mkForce (mkLiteral "rgba (40, 40, 40, 100%)");
          };
          "element" = {
            border = 0;
            padding = mkLiteral "2px";
          };
          "inputbar" = {
            spacing = 2;
            padding = mkLiteral "2px";
            text-color = lib.mkForce (mkLiteral "@normal-foreground");
            children = mkLiteral "[prompt, textbox-prompt-sep, entry, case-indicator]";
          };
          "case-indicator, entry, prompt, button" = {
            spacing = 0;
            text-color = lib.mkForce (mkLiteral "@normal-foreground");
          };
          "case-indicator" = {
            spacing = 0;
            text-color = lib.mkForce (mkLiteral "@normal-foreground");
          };
          "entry" = {
            spacing = 0;
            text-color = lib.mkForce (mkLiteral "@normal-foreground");
          };
          "prompt" = {
            spacing = 0;
            text-color = lib.mkForce (mkLiteral "@normal-foreground");
          };
          "button" = {
            spacing = 0;
            text-color = lib.mkForce (mkLiteral "@normal-foreground");
          };
          "textbox-prompt-sep" = {
            str = ":";
            expand = false;
            margin = mkLiteral "0 0.2em 0.3em 0";
            text-color = lib.mkForce (mkLiteral "@normal-foreground");
          };
          "element.normal.normal" = {
            text-color = lib.mkForce (mkLiteral "@normal-foreground");
            background-color = lib.mkForce (mkLiteral "rgba (40, 40, 40, 100%)");
          };

          "element.normal.urgent" = {
            text-color = lib.mkForce (mkLiteral "@urgent-foreground");
            background-color = lib.mkForce (mkLiteral "@urgent-background");
          };

          "element.normal.active" = {
            text-color = lib.mkForce (mkLiteral "@active-foreground");
            background-color = lib.mkForce (mkLiteral "@active-background");
          };

          "element.selected.normal" = {
            text-color = lib.mkForce (mkLiteral "@selected-normal-foreground");
            background-color = lib.mkForce (mkLiteral "@selected-normal-background");
          };

          "element.selected.urgent" = {
            text-color = lib.mkForce (mkLiteral "@selected-urgent-foreground");
            background-color = lib.mkForce (mkLiteral "@selected-urgent-background");
          };

          "element.selected.active" = {
            text-color = lib.mkForce (mkLiteral "@selected-active-foreground");
            background-color = lib.mkForce (mkLiteral "@selected-active-background");
          };

          "element.alternate.normal" = {
            text-color = lib.mkForce (mkLiteral "@alternate-normal-foreground");
            background-color = lib.mkForce (mkLiteral "@alternate-normal-background");
          };

          "element.alternate.urgent" = {
            text-color = lib.mkForce (mkLiteral "@alternate-urgent-foreground");
            background-color = lib.mkForce (mkLiteral "@alternate-urgent-background");
          };

          "element.alternate.active" = {
            text-color = lib.mkForce (mkLiteral "@alternate-active-foreground");
            background-color = lib.mkForce (mkLiteral "@alternate-active-background");
          };

          "scrollbar" = {
            border = 0;
            padding = 0;
            handle-width = mkLiteral "8px";
            width = lib.mkForce (mkLiteral "4px");
            handle-color = lib.mkForce (mkLiteral "rgba (40, 40, 40, 10%)");
          };

          "mode-switcher" = {
            border = mkLiteral "2px 0 0";
            border-color = lib.mkForce (mkLiteral "rgba (40, 40, 40, 10%)");
          };

          "button.selected" = {
            text-color = lib.mkForce (mkLiteral "@selected-normal-foreground");
            background-color = lib.mkForce (mkLiteral "@selected-normal-background");
          };

          "element-icon" = {
            text-color = lib.mkForce (mkLiteral "inherit");
            background-color = lib.mkForce (mkLiteral "inherit");
          };

          "element-text" = {
            text-color = lib.mkForce (mkLiteral "inherit");
            background-color = lib.mkForce (mkLiteral "inherit");
          };

          # TODO add more colors like this.
          # urgent = lib.mkForce (mkLiteral "${config.lib.stylix.colors.withHashtag.base0E}");
        };
      };

      services.hyprpaper.enable = lib.mkForce false; # Enabled by default with hyprland.
      wayland.windowManager.hyprland = let
        gaps_in = 6;
        gaps_out = 13;
      in {
        systemd.enable = true;
        xwayland.enable = true;
        systemd.variables = ["--all"];
        enable = true;
        extraConfig =
          /*
          hyprlang
          */
          ''
            env = NIXOS_OZONE_WL, 1
            env = XDG_CURRENT_DESKTOP, Hyprland
            env = XDG_SESSION_TYPE, wayland
            env = XDG_SESSION_DESKTOP, Hyprland
            env = GDK_BACKEND, wayland, x11, *
            env = CLUTTER_BACKEND, wayland
            env = QT_QPA_PLATFORM, wayland;xcb
            env = QT_WAYLAND_DISABLE_WINDOWDECORATION, 1
            env = QT_AUTO_SCREEN_SCALE_FACTOR, 1
            env = MOZ_ENABLE_WAYLAND, 1
            env = GTK_USE_PORTAL, 1

            monitor =, highres@highrr, auto, 1
            xwayland {
              force_zero_scaling = true
            }
          '';

        settings = {
          debug.disable_logs = true;
          # Autostart.
          exec-once = [
            "systemctl --user import-environment WAYLAND_DISPLAY &"
            "hash dbus-update-activation-environment 2>/dev/null &"
            "dbus-update-activation-environment --systemd &"

            "${pkgs.polkit-kde-agent}/bin/polkit-kde-authentication-agent-1 &"
            "${lib.getExe pkgs.swaybg} -m fill -i ${./resources/static/wallpaper.png} &"
            "udiskie &"
            # "${lib.getExe pkgs.wl-clip-persist} --clipboard both &"
            "hyprctl dispatch exec '[workspace 2 silent] $BROWSER' &"
            "hyprctl dispatch exec '[workspace 1 silent] $TERMINAL' &"
            # "wl-paste --watch cliphist store &"
          ];

          input = {
            kb_layout = "pl";
            kb_options = "caps:escape,grp:sclk_toggle";
            repeat_delay = 300;
            repeat_rate = 30;
            accel_profile = "flat";
            numlock_by_default = false;
            follow_mouse = 2;
          };

          cursor = {
            no_warps = true;
            hide_on_key_press = false;
          };

          general = {
            "$mainMod" = "SUPER";
            layout = "dwindle";
            gaps_in = gaps_in;
            gaps_out = gaps_out;
            border_size = 1;
            border_part_of_window = false;
            no_border_on_floating = false;
            "col.active_border" = lib.mkForce "rgba(${conAccentColor}FF)";
            "col.inactive_border" = lib.mkForce "rgba(${config.stylix.base16Scheme.base00}00)";
          };

          misc = {
            # Hides text on bottom of the screen.
            disable_splash_rendering = true;
            disable_hyprland_logo = true;

            disable_autoreload = true;
            animate_manual_resizes = true;
            enable_swallow = false;
            swallow_regex = "^(Alacritty)$";
          };

          dwindle = {
            force_split = 2;
            special_scale_factor = 1.0;
            split_width_multiplier = 1.0;
            use_active_for_splits = true;
            pseudotile = "yes";
            preserve_split = "yes";
          };

          master = {
            no_gaps_when_only = false;
            new_status = "slave";
            new_on_active = "after";
            new_on_top = false;
          };

          decoration = {
            rounding = 0;
            blur.enabled = false;
            drop_shadow = true;
            shadow_ignore_window = true;
            shadow_range = 10;
            shadow_render_power = 2;
            "col.shadow" = lib.mkForce "rgba(0000007F)";
          };

          animations = {
            # Fast animations.
            bezier = ["easeOutQuart, 0.190, 0.91, 0.37, 1"];
            animation = [
              "windowsIn, 1, 4, easeOutQuart, popin 0%"
              "windowsOut, 1, 4, easeOutQuart, popin 60%"
              "windowsMove, 1, 4, easeOutQuart, popin 60%"
              "windows, 1, 4, easeOutQuart"

              "fadeIn, 1, 4, easeOutQuart"
              "fadeOut, 1, 4, easeOutQuart"
              "fadeSwitch, 0, 4, easeOutQuart"
              "fadeShadow, 0, 4, easeOutQuart"
              "fadeDim, 1, 4, easeOutQuart"
              "fadeLayers, 1, 4, easeOutQuart"
              "fade, 1, 4, easeOutQuart"

              "border, 0, 4, easeOutQuart"
              "borderangle, 0, 4, easeOutQuart"

              "specialWorkspace, 1, 4, easeOutQuart, slidevert"
              "workspaces, 1, 4, easeOutQuart, slide"
            ];
          };

          # Bind accepts "flags" after "bind".
          # "e" in "binde" means that a key can be held down to repeat an action.
          # You can add multiple flags, without order like so "bindde".
          # TODO: add these binds:
          # "$mainMod SHIFT, Escape, Hard kill, exec, shutdown-script"
          #  add proper alt tab support using "hycov" plugin
          #  add descriptions to each key
          bindde = [
            "$mainMod, g, toggle [g]roup, togglegroup"
            "$mainMod, u, [u]-lock insert into group, lockactivegroup, toggle"
            "$mainMod, n, [n]ext tab, changegroupactive, f"
            "ALT, 1, Switch group tab, changegroupactive, 1"
            "ALT, 2, Switch group tab, changegroupactive, 2"
            "ALT, 3, Switch group tab, changegroupactive, 3"
            "ALT, 4, Switch group tab, changegroupactive, 4"
            "ALT, 5, Switch group tab, changegroupactive, 5"
            "ALT, 6, Switch group tab, changegroupactive, 6"
            "ALT, 7, Switch group tab, changegroupactive, 7"
            "ALT, 8, Switch group tab, changegroupactive, 8"
            "ALT, 9, Switch group tab, changegroupactive, 9"
            "ALT, 0, Switch group tab, changegroupactive, 10"
            "$mainMod, o, Move [o]ut of group, moveoutofgroup,"
            "$mainMod CTRL SHIFT, h, Move focus right, moveintogroup, l"
            "$mainMod CTRL SHIFT, l, Move focus left, moveintogroup, r"
            "$mainMod CTRL SHIFT, k, Move focus up, moveintogroup, u"
            "$mainMod CTRL SHIFT, j, Move focus down, moveintogroup, d"

            "$mainMod, q, [q]uit active, killactive,"

            "$mainMod CTRL, Space, Toggle floating, togglefloating,"

            "$mainMod, p, Toggle [p]in, pin,"
            "$mainMod, p, Toggle [p]in, togglefloating,"

            "$mainMod, f, [f]ullscreen, fullscreen"
            "$mainMod SHIFT, f, [f]akke fullscreen, fakefullscreen"

            "$mainMod, a, g[a]ps on, exec, hyprctl keyword general:gaps_in ${builtins.toString gaps_in}"
            "$mainMod, a, g[a]ps on, exec, hyprctl keyword general:gaps_out ${builtins.toString gaps_out}"
            "$mainMod SHIFT, a, g[a]ps off, exec, hyprctl keyword general:gaps_in 0"
            "$mainMod SHIFT, a, g[a]ps off, exec, hyprctl keyword general:gaps_out 0"

            "$mainMod, z, Cycle next in active workspace, cyclenext,"
            "$mainMod, x, Center active, centerwindow,"

            "$mainMod, Return, Open terminal, exec, $TERMINAL"

            "$mainMod, b, Open [b]rowser, exec, hyprctl dispatch exec '[workspace 2 silent] $BROWSER'"

            "$mainMod, Space, Program launcher, exec, pkill ${lib.getExe pkgs.rofi-wayland} || ${lib.getExe pkgs.rofi-wayland} -show drun"
            "$mainMod SHIFT, Space, Program launcher, exec, pkill ${lib.getExe pkgs.rofi-wayland} || ${lib.getExe pkgs.rofi-wayland} -show run"

            "$mainMod, c, [c]olor picker, exec, ${lib.getExe pkgs.hyprpicker} -a"

            "$mainMod, s, Toggle [s]plit, togglesplit,"

            "$mainMod, d, Set [d]windle layout, exec, hyprctl keyword general:layout \"dwindle\""
            "$mainMod, m, Set [m]aster layout, exec, hyprctl keyword general:layout \"master\""

            ", Print, Screenshot, exec, ${lib.getExe pkgs.grimblast} --freeze copy area"
            "$mainMod, e, [e]dit image, exec, ${pkgs.wl-clipboard}/bin/wl-paste | ${lib.getExe pkgs.satty} --filename -"

            "$mainMod, r, [r]ecord, exec, hyprcorder.sh"

            "ALT, Tab, Cycle programs, exec, hyprland-next-visible-client.sh next"
            "$mainMod, Tab, Open program menu, exec, ${lib.getExe pkgs.rofi-wayland} -show window"

            "$mainMod, h, Move focus right, movefocus, l"
            "$mainMod, l, Move focus left, movefocus, r"
            "$mainMod, k, Move focus up, movefocus, u"
            "$mainMod, j, Move focus down, movefocus, d"

            "$mainMod, 1, Switch workspace, workspace, 1"
            "$mainMod, 2, Switch workspace, workspace, 2"
            "$mainMod, 3, Switch workspace, workspace, 3"
            "$mainMod, 4, Switch workspace, workspace, 4"
            "$mainMod, 5, Switch workspace, workspace, 5"
            "$mainMod, 6, Switch workspace, workspace, 6"
            "$mainMod, 7, Switch workspace, workspace, 7"
            "$mainMod, 8, Switch workspace, workspace, 8"
            "$mainMod, 9, Switch workspace, workspace, 9"
            "$mainMod, 0, Switch workspace, workspace, 10"

            "$mainMod SHIFT, 1, Move to workspace, movetoworkspacesilent, 1"
            "$mainMod SHIFT, 2, Move to workspace, movetoworkspacesilent, 2"
            "$mainMod SHIFT, 3, Move to workspace, movetoworkspacesilent, 3"
            "$mainMod SHIFT, 4, Move to workspace, movetoworkspacesilent, 4"
            "$mainMod SHIFT, 5, Move to workspace, movetoworkspacesilent, 5"
            "$mainMod SHIFT, 6, Move to workspace, movetoworkspacesilent, 6"
            "$mainMod SHIFT, 7, Move to workspace, movetoworkspacesilent, 7"
            "$mainMod SHIFT, 8, Move to workspace, movetoworkspacesilent, 8"
            "$mainMod SHIFT, 9, Move to workspace, movetoworkspacesilent, 9"
            "$mainMod SHIFT, 0, Move to workspace, movetoworkspacesilent, 10"
            "$mainMod SHIFT, c, Move to empty workspace, movetoworkspace, empty"

            "$mainMod SHIFT, h, Move left to workspace, movewindoworgroup, l"
            "$mainMod SHIFT, l, Move right to workspace, movewindoworgroup, r"
            "$mainMod SHIFT, k, Move up to workspace, movewindoworgroup, u"
            "$mainMod SHIFT, j, Move down to workspace, movewindoworgroup, d"

            "$mainMod CTRL, h, Resize window left, resizeactive, -100 0"
            "$mainMod CTRL, l, Resize window right, resizeactive, 100 0"
            "$mainMod CTRL, k, Resize window up, resizeactive, 0 -100"
            "$mainMod CTRL, j, Resize window down, resizeactive, 0 100"

            "$mainMod ALT, h, Move floating left, moveactive, -100 0"
            "$mainMod ALT, l, Move floating right, moveactive, 100 0"
            "$mainMod ALT, k, Move floating up, moveactive, 0 -100"
            "$mainMod ALT, j, Move floating down, moveactive, 0 100"

            "$mainMod, Equal, Volume down, exec, ${lib.getExe pkgs.pamixer} -i 2"
            "$mainMod, Minus, Volume up, exec, ${lib.getExe pkgs.pamixer} -d 2"

            "$mainMod, mouse_down, Scroll workspace, workspace, e-1"
            "$mainMod, mouse_up, Scroll workspace, workspace, e+1"
          ];

          # mouse binding
          bindm = [
            "$mainMod, mouse:272, movewindow"
            "$mainMod, mouse:273, resizewindow"
          ];
          layerrule = [
            # "layers, 1, 4, default, slide top"
            "dimaround, rofi"
            "animation slide top, rofi"
          ];
          windowrule = [
            "workspace 1      , title:Terminal"
            "workspace 2      , title:Web"
            "workspace 3      , title:Development"
            "workspace 4      , title:Chat"
            "workspace 8      , title:Steam"
            "workspace 10     , title:passwordManager"

            "pin, ripdrag"
            "float, ripdrag"

            "float            , imv"
            "center           , imv"
            "size 1200 725    , imv"

            "float            , mpv"
            "center           , mpv"
            "size 1200 725    , mpv"

            "tile             , Aseprite"

            "pin              , rofi"
            "float            , rofi"
            "noborder         , rofi"

            "tile             , neovide"

            "idleinhibit focus, mpv"

            "float            ,udiskie"
          ];

          windowrulev2 = [
            # Needed for gloss window to tile and not focus.
            "tile, class:^()$"
            "noinitialfocus, class:^()$"

            # Shadow only for floating windows
            "noshadow, floating:0"

            "suppressevent maximize, class:.*"

            "workspace 2 silent, class:^(firefox)$"
            "workspace 2 silent, class:^(librewolf)$"

            "workspace 8 silent, class:^(Steam|steam|steam_app_.*)$, title:^((?!notificationtoasts.*).)*$"
            "workspace 8 silent, title:^(.*Steam[A-Za-z0-9\s]*)$"

            "float, title:^(Picture-in-Picture)$"
            "opacity 1.0 override 1.0 override, title:^(Picture-in-Picture)$"
            "pin, title:^(Picture-in-Picture)$"
            "float, title:^(Firefox — Sharing Indicator|Wine System Tray)$"
            "size 0 0, title:^(Firefox — Sharing Indicator|Wine System Tray)$"

            "opacity 1.0 override 1.0 override, title:^(.*imv.*)$"
            "opacity 1.0 override 1.0 override, title:^(.*mpv.*)$"

            "idleinhibit focus, class:^(mpv)$"
            "idleinhibit fullscreen, class:^(librewolf)$"

            "opacity 1.0 override 1.0 override, class:(Aseprite)"
            "opacity 1.0 override 1.0 override, class:(Unity)"

            "float, class:^(pavucontrol)$"
            "float, class:^(SoundWireServer)$"
            "float, class:^(.sameboy-wrapped)$"

            "float, class:^(file_progress)$"
            "float, class:^(confirm)$"
            "float, class:^(dialog)$"
            "float, class:^(download)$"
            "float, class:^(notification)$"
            "float, class:^(error)$"
            "float, class:^(confirmreset)$"
            "float, title:^(Open File)$"
            "float, title:^(branchdialog)$"
            "float, title:^(Confirm to replace files)$"
            "float, title:^(File Operation Progress)$"
          ];
        };
      };

      # Extra Configs
      xdg.enable = true;

      xdg.configFile."awesome/" = {
        source = ./resources/awesome;
        recursive = true;
      };

      xdg.configFile."wallpaper.png" = {
        source = ./resources/static/wallpaper.png;
      };

      xdg.configFile."sx/sxrc" = {
        executable = true;
        text = ''
          $TERMINAL &
          xrandr -r ${builtins.toString conRefresh-rate}
          exec awesome
        '';
      };

      xdg.configFile."fourmolu.yaml".source = (pkgs.formats.yaml {}).generate "fourmoluExtraConfigDIY" {
        indentation = 2;
        respectful = false;
        in-style = "left-align";
        comma-style = "trailing";
        indent-wheres = true;
        let-style = "inline";
        haddock-style = "single-line";
      };

      xdg.configFile."stylish-haskell/config.yaml".text = ''
        steps:
          - simple_align:
              cases: always
              top_level_patterns: always
              records: always
              multi_way_if: always
          - imports:
              align: global
              list_align: after_alias
              pad_module_names: true
              long_list_align: inline
              empty_list_align: inherit
              list_padding: 4
              separate_lists: true
              space_surround: false
              post_qualify: false
              group_imports: false
              group_rules:
                - match: ".*"
                  sub_group: "^[^.]+"
          - language_pragmas:
              style: vertical
              align: true
              remove_redundant: true
              language_prefix: LANGUAGE
          - trailing_whitespace: {}
        columns: 100
        newline: native
        cabal: true
      '';

      xdg.configFile."neovide/neovide.toml".source = (pkgs.formats.toml {}).generate "neovideExtraConfigDIY" {
        font = {
          normal = ["JetBrains Mono"];
          size = 13;
        };
      };
    };
  };

  system.stateVersion = "24.05"; # Dont change # CHANGE IT ON UPDATE NO BALLS
}
