{
  pkgs,
  lib,
  inputs,
  host,
  pkgs-unstable,
  ...
}: let
  constants = {
    username = "samsepi0l";
    accentColor = "7d8618"; #7d8618 Hacky!!! Add extra color to stylix.
    home = "/home/${constants.username}";
    flake-path = "${constants.home}/nixos";

    refresh-rate = 144;
    screen-width = 1920;
    screen-height = 1080;
  };
in {
  imports = [
    inputs.stylix.nixosModules.stylix
    inputs.home-manager.nixosModules.default
  ];

  #######################
  # Essential or basic. #
  #######################

  # specialisation = {
  #   KDE.configuration = {
  #     services.xserver.desktopManager.plasma5.enable = true;
  #     services.xserver.displayManager = {
  #       sddm.enable = true;
  #     };
  #     services.displayManager.defaultSession = "plasma";
  #     environment.plasma6.excludePackages = with pkgs.kdePackages; [
  #       plasma-browser-integration
  #       konsole
  #     ];
  #   };
  # };

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

  # DNS
  networking.nameservers = ["9.9.9.9" "149.112.112.112"];

  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "Europe/Warsaw";

  services.libinput.enable = true;
  services.libinput.mouse.accelProfile = "flat";
  services.libinput.mouse.accelSpeed = "-0.9";

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
  users.users.${constants.username} = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
  };

  ###################
  # NixOS programs. #
  ###################

  # Allowed unfree packages
  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [
      "steam"
      "steam-original"
      "steam-run"
    ];

  # Needed here and in home manager.
  programs.hyprland = {
    enable = true;
  };

  # For distrobox.
  virtualisation.podman.enable = true;
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

    distrobox # Allows to use better distros on NixOS
    podman # Dependency for distrobox

    # For minecraft
    jdk8 # Java 8
    # Game launcher
    lutris
    wineWowPackages.waylandFull
    wineWowPackages.stable
    winetricks

    # Other.
    wl-clipboard # Wayland xclip
    cliphist # Wayland clipboard manager
    jq # Json parser, needed for "hyprland-next-visible-client.sh"
    # Treesitter needs it.
    patool # Universal archiver
    libqalculate # Calculator
    conky # Hardware monitor
    vim # Text editor
    zed-editor # Another text editor
    wget # Downloader
    eza # ls rewrite
    trashy # Cli trashcan
    tmux # Terminal multiplexer
    dash # Lightweight shell
    git # Source control
    fzf # Fuzzy finder
    ripgrep # Multithreaded grep
    gnumake # C compiling
    gcc # C compiling
    fd # Find alternative
    libnotify # Notifications (notify-send)

    # For Hyprland
    qt5.qtwayland
    qt6.qtwayland
    libsForQt5.qt5ct
    libsForQt5.qt5.qtwayland
    kdePackages.qtwayland

    xdg-utils # Includes xdg-open

    # These are filepickers and whatnot
    xdg-desktop-portal-gtk
    xdg-desktop-portal-hyprland
    hyprland-protocols

    # Shellscripts.
    (writeShellScriptBin "hyprland-next-visible-client.sh" (builtins.readFile ./resources/scripts/hyprland-next-visible-client.sh))

    (writeShellScriptBin "vmrss" (builtins.readFile ./resources/scripts/vmrss.sh))

    (writeShellScriptBin
      "sgamescope" # [s]team [gamescope]

      ''
        gamescope -w ${builtins.toString constants.screen-width} -W ${builtins.toString constants.screen-width} -h ${builtins.toString constants.screen-height} -H ${builtins.toString constants.screen-height} -r ${builtins.toString constants.refresh-rate} -f steam
      '')

    (writeShellScriptBin
      "hard-clean-nix"
      ''
        nix-collect-garbage --delete-older-than 5d
        sudo nix-collect-garbage --delete-older-than 5d
        nix store optimise
        sudo nix store optimise
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
        pushd ${constants.home}/screencaptures/
        ${lib.getExe pkgs.ripdrag} $(ls -t | head -n1)
        popd
        exit
      else
        monitor=$(hyprctl activeworkspace -j | jq -r '.monitor')
        notify-send "Recording starting on $monitor."
        ${lib.getExe pkgs.wl-screenrec} -b "1 MB" -o $monitor -f ${constants.home}/screencaptures/$(date  +"%y.%m.%d-%H:%M").mp4
        fi
    '')
  ];

  ########################
  # Hardware or drivers. #
  ########################

  # Most software has the HIP libraries hard-coded. You can work around it on NixOS by using:
  systemd.tmpfiles.rules = [
    "L+    /opt/rocm/hip   -    -    -     -    ${pkgs.rocmPackages.clr}"
  ];

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      vaapiVdpau
      libvdpau-va-gl
      rocm-opencl-icd
      rocm-opencl-runtime
    ];
    extraPackages32 = with pkgs.pkgsi686Linux; [libva];
  };

  services.xserver.videoDrivers = ["amdgpu"];

  ##########
  # Games. #
  ##########

  # This is the command for running all 3 programs at once that u put into steam
  # gamemoderun gamescope -w 1920 -h 1080 -f -- mangohud %command%
  programs.steam = {
    enable = true;
    gamescopeSession.enable = true;
    # Load the extest library into Steam, to translate X11 input events to uinput events (for using Steam Input on Wayland).
    # extest.enable = true;
    extraCompatPackages = [
      pkgs-unstable.proton-ge-bin
    ];
    extraPackages = [
      pkgs.gamescope
      pkgs.gamemode
      pkgs.libkrb5
      pkgs.keyutils
      pkgs.xorg.libXcursor
      pkgs.xorg.libXi
      pkgs.xorg.libXinerama
      pkgs.xorg.libXScrnSaver
      pkgs.libpng
      pkgs.libpulseaudio
      pkgs.libvorbis
      pkgs.stdenv.cc.cc.lib
      pkgs.libkrb5
      pkgs.keyutils
    ];
  };

  programs.gamemode.enable = true;

  programs.gamescope = {
    enable = true;
    capSysNice = true;
  };

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

  # X11 window manager for games
  services.xserver.windowManager.awesome = {
    enable = true;
  };

  # File synchronization.
  services.syncthing = {
    enable = true;
    settings.options.relaysEnabled = false;
    openDefaultPorts = true;
    overrideFolders = false;
    overrideDevices = false;
    user = constants.username;
    dataDir = constants.home;
    settings.devices = {
      phone = {
        addresses = [
          "tcp://192.168.1.10:22000"
        ];
        id = "Z7AOC2O-CYXT6XV-Y67O5RB-VAXE2JT-JV36AMW-KWQ3U6Z-PVTINXB-IQ2UHQ7";
        autoAcceptFolders = true;
      };
    };
  };

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

  # Enable sound with low latency.
  hardware.pulseaudio.enable = false;
  # RealtimeKit service, which hands out realtime scheduling priority to user processes on demand. For example, the PulseAudio server uses this to acquire realtime priority.
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    wireplumber.enable = true;
    pulse.enable = true;
    alsa.enable = true;
    # Quirky low latency ig for gaming (id rather not)
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
    FLAKE = "${constants.flake-path}"; # For nix helper.
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
      jetbrains-mono
      (nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
      noto-fonts-cjk-serif
      noto-fonts-cjk-sans
      noto-fonts-emoji
      noto-fonts
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
  stylix.image = ./resources/static/wallpaper.png;
  stylix.targets.grub.useImage = true;
  stylix.cursor.package = pkgs.capitaine-cursors-themed;
  stylix.cursor.name = "Capitaine Cursors (Gruvbox)";
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
      package = pkgs.nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];};
      name = "Symbols Nerd Font";
    };
  };

  stylix.fonts.sizes = {
    applications = 12;
    terminal = 13;
    desktop = 13;
    popups = 13;
  };

  stylix.opacity = {
    applications = 0.5;
    terminal = 0.8;
    desktop = 0.8;
    popups = 0.8;
  };

  # Supposedly fixes some themeing/cursor issues might be useless.
  programs.dconf.enable = true;

  ################
  # HOME MANAGER #
  ################

  home-manager = {
    extraSpecialArgs = {inherit inputs pkgs-unstable;};
    backupFileExtension = "backup"; # h-m breaks without it.
    users.${constants.username} = {
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
        username = "${constants.username}";
        homeDirectory = "${constants.home}";
        stateVersion = "24.05"; # Dont change

        shellAliases = {
          "tree" = "${lib.getExe pkgs.eza} --group-directories-first --tree";
          "ls" = "${lib.getExe pkgs.eza}";
          "pmem" = "vmrss"; # [p]rocess [mem]ory
          "la" = "${lib.getExe pkgs.eza} -a";
          "ll" = "${lib.getExe pkgs.eza} -l";
          "rt" = "${lib.getExe pkgs.trashy}";
          "qcalc" = "${lib.getExe pkgs.libqalculate}";
          "shutdown" = "poweroff";
          "nhoffline" = "nh os switch ${constants.flake-path} -- --option substitute false";
          "search" = "sudo find / -maxdepth 99999999 2>/dev/null | ${lib.getExe pkgs.fzf} -i -q $1";
        };

        sessionVariables = {
          # Default programs.
          PAGER = lib.getExe pkgs.moar;
          # Systemd is retarded and doesnt use normal pager variable :DDDDD
          SYSTEMD_PAGER = lib.getExe pkgs.moar;
          OPENER = lib.getExe' pkgs.xdg-utils "xdg-open";
          VISUAL = "nvim";
          EDITOR = "nvim";
          SUDO_EDITOR = "nvim";
          TERMINAL = lib.getExe pkgs.alacritty;
          TERMINAL_PROG = lib.getExe pkgs.alacritty;
          BROWSER = lib.getExe pkgs-unstable.librewolf;
          # Firefox hardware decode.
          MOZ_X11_EGL = 1;
          NO_AT_BRIDGE = 1;
          # Unreal engine .net cli tool turn off telemetry.
          DOTNET_CLI_TELEMETRY_OPTOUT = "true";
          QT_QPA_PLATFORMTHEME = "qt5ct";
        };

        # Home packages, home manager packages, user packages
        packages = with pkgs; [
          # GUI.
          prismlauncher # Minecraft launcher
          keepassxc # Password manager
          tauon # Music player
          foliate # Ebook reader
          anki # Flashcards
          qbittorrent # Torrent client
          libreoffice # Office
          neovide # Neovim gui
          rofi-wayland # App launcher
          # hydrus # File manager

          # For minecraft
          jdk11

          # Command line.
          bc # Gnu calculator, needed for vmrss
          pulsemixer # Volume control
          zoxide # Cd alternative
          ffmpeg # Video and magic editor
          hyprpicker # Color picker
          slurp # Screenshot assistant
          swappy # Quick drawing on images
          termdown # Timer
          tldr # Man alternative
          htop # TUI task manager
          moar # Pager

          # Unstable
          pkgs-unstable.krita # Painting
          pkgs-unstable.inkscape # Painting
          pkgs-unstable.librewolf # Browser
          pkgs-unstable.blender # 3D graphics
          pkgs-unstable.gomuks # TUI matrix client
        ];

        # Binary blobs.
        sessionPath = ["${constants.home}/bin"]; # Add ~/bin to path.
        file."bin/tmux-mem-cpp".source = ./resources/static/tmux-mem-cpp;

        # This allows for semi-declarative configuration.
        activation.configure-krita = lib.hm.dag.entryAfter ["writeBoundary"] ''
          if ! [ -f "${config.xdg.configHome}/kritarc" ]; then
              mkdir -p ${config.xdg.configHome}
              run cp $VERBOSE_ARG "${builtins.toPath ./resources/krita-extraConfig}" "${config.xdg.configHome}/kritarc"
          fi
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
      programs.nix-index.enable = true;
      programs.bash.enable = true;
      programs.zoxide.enable = true;
      programs.home-manager.enable = true;
      programs.git-credential-oauth.enable = true;

      programs.mangohud = {
        enable = true;
        enableSessionWide = false;
      };

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
          save-position-on-quit = true;
          hwdec = true;
          ytdl-format = "bestvideo+bestaudio/best";
          slang = "fin,fi,fi-fi,eng,en,en-en,en-orig";
          sub-auto = "all";
          sub-visibility = "yes";
          sub-auto-exts = "srt,ass,txt";
          speed = 1;
          keep-open = true;
          loop-file = "inf";
          sub-font = "${config.stylix.fonts.serif.name}";
          sub-ass-override = "force";
          sub-ass-force-style = "${config.stylix.fonts.serif.name}";
          sub-ass-line-spacing = 1;
          sub-ass-hinting = "normal";
          sub-border-size = 2;
          sub-pos = 90;
          sub-font-size = 40;
          sub-color = "${config.lib.stylix.colors.withHashtag.base07}";
          sub-shadow-color = "${config.lib.stylix.colors.withHashtag.base00}";
          sub-shadow-offset = 2;
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

      programs.tmux = {
        enable = true;
        keyMode = "vi";
        prefix = "C-a";
        mouse = true; # Allows you to scroll a terminal
        escapeTime = 0; # Delay after pressing escape
        baseIndex = 1;
        historyLimit = 1000; # Alacritty already holds history
        extraConfig = ''
          #No hanging sessions.
          set-option -sg destroy-unattached

          # For vim autoread.
          set-option -g focus-events on

          # For truecolor inside tmux
          set -g default-terminal 'tmux-256color'
          set -as terminal-overrides ",alacritty*:Tc"

          # Easy-to-remember split pane commands.
          bind | split-window -h
          bind - split-window -v
          unbind '"'
          unbind %

          # I don't know, read the docs.
          setw -g monitor-activity on
          set -g visual-activity on

          # Moving between panes with vim movement keys.
          bind h select-pane -L
          bind j select-pane -D
          bind k select-pane -U
          bind l select-pane -R
          # moving between windows with vim movement keys.
          bind -r C-h select-window -t :-
          bind -r C-l select-window -t :+
          # Resize panes with vim movement keys.
          bind -r H resize-pane -L 5
          bind -r J resize-pane -D 5
          bind -r K resize-pane -U 5
          bind -r L resize-pane -R 5

          # Status line.
          set-option -sg status on
          set-option -sg status-interval 30
          set-option -sg status-justify left
          set-option -sg status-keys vi
          set-option -sg status-left ""
          set-option -sg status-left-length 10
          set-option -sg status-left-style default
          set-option -sg status-position bottom
          set-option -sg status-right "#(tmux-mem-cpp) %Y-%m-%d (%Ob %a) %H:%M"
          set-option -sg status-right-length 45
          set-option -sg status-right-style default
          set-option -sg status-style fg=green,bg=default

          # y and p as in vim.
          set-option -sg set-clipboard on
          bind Escape copy-mode
          # Bind p paste-buffer.
          bind-key -T copy-mode-vi 'p' send -X paste-buffer
          bind-key -T copy-mode-vi 'v' send -X begin-selection
          bind-key -T copy-mode-vi 'y' send-keys -X copy-pipe-and-cancel 'xclip -se c -i'
          bind-key -T copy-mode-vi 'Space' send -X halfpage-down
          bind-key -T copy-mode-vi 'Bspace' send -X halfpage-up
        '';
      };

      programs.fzf = {
        enable = true;
        tmux.enableShellIntegration = true;
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
            lines = 0; # Use tmux for scrolling.
          };
        };
      };

      programs.alacritty = {
        enable = true;
        settings = {
          scrolling.history = 0; # Disables scrolling, use tmux.
          window.dynamic_padding = true;
          window.dynamic_title = true;
          scrolling.multiplier = 5;
          selection.save_to_clipboard = false;
          cursor.style.shape = "Underline";
          cursor.style.blinking = "on";
          cursor.unfocused_hollow = false;
          window.padding = {
            x = 8;
            y = 8;
          };
          font.offset.y = 3; # Line spacing
        };
      };

      programs.nixvim = {
        enable = true;
        extraPackages = with pkgs; [
          rust-analyzer # Rust LSP
          luajitPackages.jsregexp # Needed for luasnip
          vim-language-server
          typos-lsp
          vale # Linter
          haskellPackages.ormolu # Haskell formatter
          marksman # Markdown LSP
          haskell-language-server # Haskell LSP
          python312Packages.python-lsp-server
          python312Packages.pyls-isort # Python import sort
          python312Packages.pycodestyle # Python complainer
          python312Packages.pylsp-mypy # Static checker plugin
          python312Packages.jedi # Autocomplete plugin
          python312Packages.mccabe # Flake8 plugin
          python312Packages.flake8 # Pylsp plugin
          python312Packages.pylsp-rope # Refactoring plugin
          python312Packages.pyflakes # Python linter
          sumneko-lua-language-server
          nodePackages.bash-language-server
          stylua # Lua formatter
          isort
          shellcheck # Bash linter
          hadolint # Docker linter
          nodePackages.jsonlint
          mypy # Python type checker
          black # Python formatter
          yapf # Python formatter
          ruff # Python linter
          hlint # Haskell linter

          # Nix
          deadnix # Linter
        ];

        highlightOverride = {
          # hi noCursor blend=100 cterm=strikethrough
          ModeMsg.fg = "#${constants.accentColor}";
          FloatBorder.fg = "#${constants.accentColor}";
          noCursor.blend = 100;
          statusline.bg = "NONE";
          statusline.fg = "#${constants.accentColor}";
          CursorLineNr.fg = "#${constants.accentColor}";
          CursorLineNr.bg = "${config.lib.stylix.colors.withHashtag.base01}"; # Gray numberline
          MsgArea.fg = "#${constants.accentColor}";
          MiniIndentscopeSymbol.fg = "${config.lib.stylix.colors.withHashtag.base01}"; # Gray indentline
        };

        opts = {
          # Indents.
          expandtab = true;
          tabstop = 2;
          shiftwidth = 2;
          softtabstop = 2;
          autoindent = true;
          breakindent = true; # Indent when wrapping

          # Wrapping.
          wrap = false;

          # Delay on switching to normal mode.
          ttimeoutlen = 0;

          # Incremental search.
          hlsearch = true;
          incsearch = true;
          updatetime = 100;

          # Relative numberline on the left.
          number = true;
          relativenumber = true;

          # Color current line number.
          cursorline = true;
          cursorlineopt = "number";

          # Smartcase search and ripgrep.
          ignorecase = true;
          smartcase = true;
          grepprg = "rg --vimgrep";
          grepformat = "%f:%l:%c:%m";

          # Folds.
          foldmethod = "expr";
          foldexpr = "nvim_treesitter#foldexpr()";
          # foldmethod = "indent";
          foldlevel = 1;
          foldclose = "all";
          foldenable = false; # Disable folding at startup.

          # More space.
          cmdheight = 0;

          # Puts error messages on the number line.
          signcolumn = "number";

          # Show some whitespace.
          list = true;
          listchars = "tab:▸ ,trail:·,nbsp:␣";

          # Better completion.
          completeopt = ["menuone" "noselect" "noinsert"];

          # Always keep 8 lines above/below cursor unless at start/end of file.
          scrolloff = 8;

          # Use conform-nvim for gq formatting. ('formatexpr' is set to vim.lsp.formatexpr(),
          # so you can format lines via gq if the language server supports it).
          formatexpr = "v:lua.require'conform'.formatexpr()";

          # (https://neovim.io/doc/user/options.html#'laststatus')
          laststatus = 3;
        };
        globals = {
          mapleader = " ";

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
          neovide_refresh_rate = constants.refresh-rate;
          neovide_cursor_vfx_mode = "ripple";
          neovide_cursor_animation_length = 0.08;
          neovide_cursor_smooth_blink = true;
          neovide_floating_shadow = false;
          neovide_cursor_animate_command_line = true;
          neovide_cursor_animate_in_insert_mode = true;
        };
        extraConfigVim = builtins.readFile ./resources/nvim-extraConfig.vim;
        extraConfigLua = ''
          if vim.g.neovide then
            vim.cmd[[colorscheme gruvbox-material]]
            vim.o.background = "dark"
            vim.o.guifont = "JetBrains Mono:h13:#e-antialias:#h-slight"
            vim.cmd [[ hi Normal guibg=${config.lib.stylix.colors.withHashtag.base00} ]]
          end

          -- Transparent hover
          vim.api.nvim_set_hl(0, 'NormalFloat', { link = 'Normal', })

          -- Leap bidirectional search
          vim.keymap.set('n',        's', '<Plug>(leap)')
          vim.keymap.set('n',        'S', '<Plug>(leap-from-window)')
          vim.keymap.set({'x', 'o'}, 's', '<Plug>(leap-forward)')
          vim.keymap.set({'x', 'o'}, 'S', '<Plug>(leap-backward)')

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
        '';
        package = pkgs.neovim-unwrapped;
        clipboard.register = "unnamedplus";

        colorschemes.base16.enable = lib.mkForce false;
        # colorschemes.gruvbox.enable = true;
        colorscheme = "gruvbox-material";

        extraPlugins = [
          (pkgs.vimUtils.buildVimPlugin {
            name = "cutlass.nvim";
            src = inputs.nvim-plugin-cutlass;
          })
          pkgs.vimPlugins.gruvbox-material
        ];

        plugins = {
          nix.enable = true;
          surround.enable = true;
          rainbow-delimiters.enable = true;
          direnv.enable = true;
          spider.enable = true;
          comment.enable = true;

          nvim-ufo = {
            enable = true;
          };

          harpoon = {
            enable = true;
            enableTelescope = true;
            tmuxAutocloseWindows = true;
          };

          nvim-colorizer = {
            enable = true;
            fileTypes = let
              css = {css = true;};
            in [
              "*"
              ({language = "css";} // css)
              ({language = "scss";} // css)
              ({language = "sass";} // css)
              ({language = "less";} // css)
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

          leap = {
            enable = true;
            addDefaultMappings = false;
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
                use_languagetree = true;
                additional_vim_regex_highlighting = ["haskell"];
              };
            };
          };

          lsp = {
            enable = true;
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
              typos-lsp.enable = true;

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

          conform-nvim = {
            enable = true;
            extraOptions = {
              lsp_fallback = true;
            };
            formattersByFt = {
              # Conform will run multiple formatters sequentially.
              python = ["isort" "black" "yapf"];
              haskell = ["ormolu"];
              nix = ["alejandra"];
              lua = ["stylua"];
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
              "<Leader>l" = "+[l]sp";
              "<Leader>t" = "+[t]elescope";
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

          luasnip.enable = true;
          cmp-nvim-lsp.enable = true;
          cmp-nvim-lsp-signature-help.enable = true;
          cmp = {
            enable = true;
            autoEnableSources = true;
            settings = {
              autocomplete = true;
              snippet.expand = ''
                function(args)
                  require('luasnip').lsp_expand(args.body)
                end
              '';
              sources = [
                {name = "nvim_lsp";}
                {name = "luasnip";}
                {name = "vsnip";}
                {name = "treesitter";}
                {name = "nvim_lsp_signature_help";}
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
            mode = ["n"];
            action = "Gzz";
            key = "G";
            options.desc = "Center bottom";
          }
          {
            mode = ["n"];
            action = "ggzz";
            key = "gg";
            options.desc = "Center top";
          }
          {
            mode = ["n"];
            action = "gj";
            key = "j";
            options.desc = "Move down through wrapped line";
          }
          {
            mode = ["n"];
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

          # Plugin garbage.
          {
            mode = ["n"];
            key = "<Leader>c";
            action = "<cmd>lua require('conform').format({ timeout_ms = 500 })<CR>";
            options = {
              desc = "[C]onform";
            };
          }

          {
            mode = ["n"];
            key = "gm";
            action = "m";
            options = {
              desc = "Set mark";
            };
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
            action = "<cmd>lua vim.lsp.buf.rename()<CR>";
            key = "<Leader>rn";
            options.desc = "[r]e[n]ame";
          }
          {
            action = "<cmd>lua vim.diagnostic.open_float()<CR>";
            key = "<Leader>e";
            options.desc = "Open diagnostic";
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
            normal-background = lib.mkForce (mkLiteral "rgba (0, 0, 0, 0%)");
            alternate-normal-background = lib.mkForce (mkLiteral "rgba (0, 0, 0, 0%)");
            gruvbox-dark-bg0 = lib.mkForce (mkLiteral "rgba (21, 22, 3, 60%)");
            gruvbox-dark-bg0-soft = lib.mkForce (mkLiteral "rgba (21, 22, 3, 60%)");
          };
          "window" = {
            background-color = lib.mkForce "@background";
            border = 2;
            padding = 5;
          };
          "mainbox" = {
            border = 0;
            padding = 0;
          };
          "message" = {
            border = mkLiteral "2px 0 0";
            border-color = lib.mkForce (mkLiteral "rgba (21, 22, 3, 10%)");
            padding = mkLiteral "1px";
          };
          "textbox" = {
            highlight = "@highlight";
            text-color = lib.mkForce "@foreground";
          };
          "listview" = {
            border = mkLiteral "0px solid 0 0";
            padding = mkLiteral "0px 0 0";
            border-color = lib.mkForce (mkLiteral "rgba (21, 22, 3, 10%)");
            spacing = mkLiteral "0px";
          };
          "element" = {
            border = 0;
            padding = mkLiteral "2px";
          };
          "inputbar" = {
            spacing = 0;
            text-color = lib.mkForce (mkLiteral "@normal-foreground");
            padding = mkLiteral "2px";
          };

          # TODO add more colors like this.
          # urgent = lib.mkForce (mkLiteral "${config.lib.stylix.colors.withHashtag.base0E}");
        };
      };

      services.hyprpaper.enable = lib.mkForce false; # Enabled by default with hyprland.
      wayland.windowManager.hyprland = {
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

        plugins = [
          pkgs.hyprlandPlugins.hyprexpo
        ];

        settings = {
          debug.disable_logs = false;
          # Autostart.
          exec-once = [
            "systemctl --user import-environment WAYLAND_DISPLAY &"
            "hash dbus-update-activation-environment 2>/dev/null &"
            "dbus-update-activation-environment --systemd &"

            "${pkgs.polkit-kde-agent}/bin/polkit-kde-authentication-agent-1 &"
            "${lib.getExe pkgs.swaybg} -m fill -i ${./resources/static/wallpaper.png} &"
            "${lib.getExe' pkgs.udiskie "udiskie"} &"
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
            follow_mouse = 0;
            sensitivity = -0.9;
          };

          cursor = {
            no_warps = true;
            hide_on_key_press = true;
          };

          general = {
            "$mainMod" = "SUPER";
            layout = "dwindle";
            gaps_in = 15;
            gaps_out = 35;
            border_size = 1;
            border_part_of_window = false;
            no_border_on_floating = false;
            "col.active_border" = lib.mkForce "rgba(${constants.accentColor}FF)";
            "col.inactive_border" = lib.mkForce "rgba(${config.stylix.base16Scheme.base00}00)";
          };

          misc = {
            # Hides text on bottom of the screen.
            disable_splash_rendering = true;
            disable_hyprland_logo = true;

            disable_autoreload = true;
            animate_manual_resizes = true;
            enable_swallow = true;
            swallow_regex = "^(a|A)lacritty$";
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
            # These options will be needed after hyprland update:
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
            animation = [
              "border,1,1,default"
              "fade,1,2,default"
              "windows,1,1.5,default,popin 80%"
              "workspaces,1,1,default,slide"
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
            # This throws an invalid dispatcher error
            # but it seems good to me and it works.
            "$mainMod         , w           , Show [w]orkspaces              , hyprexpo:expo        , toggle"
            "$mainMod         , q           , [q]uit active                  , killactive           ,"

            "$mainMod         , CTRL + Space, Toggle floating                , togglefloating       ,"

            "$mainMod         , f           , [f]ullscreen                   , fullscreen"
            "$mainMod SHIFT   , f           , [f]akke fullscreen              , fakefullscreen"

            "$mainMod         , g           , [g]aps on                      , exec                 , hyprctl keyword general:gaps_in 15"
            "$mainMod         , g           , [g]aps on                      , exec                 , hyprctl keyword general:gaps_out 35"
            "$mainMod SHIFT   , G           , [G]aps off                     , exec                 , hyprctl keyword general:gaps_in 0"
            "$mainMod SHIFT   , G           , [G]aps off                     , exec                 , hyprctl keyword general:gaps_out 0"

            "$mainMod         , z           , Cycle next in active workspace , cyclenext            ,"
            "$mainMod         , x           , Center active                  , centerwindow         ,"

            "$mainMod         , Return      , Open terminal                  , exec                 , $TERMINAL"

            "$mainMod         , b           , Open [b]rowser                 , exec                 , hyprctl dispatch exec '[workspace 2 silent] $BROWSER'"

            "$mainMod         , Space       , Program launcher               , exec                 , pkill ${lib.getExe pkgs.rofi-wayland} || ${lib.getExe pkgs.rofi-wayland} -show drun"

            "$mainMod         , ALT + Space       , Program launcher               , exec                 , pkill ${lib.getExe pkgs.rofi-wayland} || ${lib.getExe pkgs.rofi-wayland} -show run"

            "$mainMod         , c           , [c]olor picker                 , exec                 , ${lib.getExe pkgs.hyprpicker} -a"

            "$mainMod         , s           , Toggle [s]plit                 , togglesplit          ,"

            "$mainMod         , d           , Set [d]windle layout           , exec                 , hyprctl keyword general:layout \"dwindle\""
            "$mainMod         , m           , Set [m]aster layout            , exec                 , hyprctl keyword general:layout \"master\""

            "                 , Print       , Screenshot                     , exec                 , ${lib.getExe pkgs.hyprshot} -m region --clipboard-only"
            "$mainMod         , e           , [e]dit image                   , exec                 , ${pkgs.wl-clipboard}/bin/wl-paste | ${lib.getExe pkgs.satty} --filename -"

            "$mainMod         , r           , [r]ecord                       , exec                 , hyprcorder.sh"

            "ALT              , Tab         , Cycle programs                 , exec                 , hyprland-next-visible-client.sh next"
            "$mainMod         , Tab         , Open program menu              , exec                 , ${lib.getExe pkgs.rofi-wayland} -show window"

            "$mainMod         , h           , Move focus right               , movefocus            , l"
            "$mainMod         , l           , Move focus left                , movefocus            , r"
            "$mainMod         , k           , Move focus up                  , movefocus            , u"
            "$mainMod         , j           , Move focus down                , movefocus            , d"

            "$mainMod         , 1           , Switch workspace               , workspace            , 1"
            "$mainMod         , 2           , Switch workspace               , workspace            , 2"
            "$mainMod         , 3           , Switch workspace               , workspace            , 3"
            "$mainMod         , 4           , Switch workspace               , workspace            , 4"
            "$mainMod         , 5           , Switch workspace               , workspace            , 5"
            "$mainMod         , 6           , Switch workspace               , workspace            , 6"
            "$mainMod         , 7           , Switch workspace               , workspace            , 7"
            "$mainMod         , 8           , Switch workspace               , workspace            , 8"
            "$mainMod         , 9           , Switch workspace               , workspace            , 9"
            "$mainMod         , 0           , Switch workspace               , workspace            , 10"

            "$mainMod SHIFT   , 1           , Move to workspace              , movetoworkspacesilent, 1"
            "$mainMod SHIFT   , 2           , Move to workspace              , movetoworkspacesilent, 2"
            "$mainMod SHIFT   , 3           , Move to workspace              , movetoworkspacesilent, 3"
            "$mainMod SHIFT   , 4           , Move to workspace              , movetoworkspacesilent, 4"
            "$mainMod SHIFT   , 5           , Move to workspace              , movetoworkspacesilent, 5"
            "$mainMod SHIFT   , 6           , Move to workspace              , movetoworkspacesilent, 6"
            "$mainMod SHIFT   , 7           , Move to workspace              , movetoworkspacesilent, 7"
            "$mainMod SHIFT   , 8           , Move to workspace              , movetoworkspacesilent, 8"
            "$mainMod SHIFT   , 9           , Move to workspace              , movetoworkspacesilent, 9"
            "$mainMod SHIFT   , 0           , Move to workspace              , movetoworkspacesilent, 10"
            "$mainMod SHIFT    , c           , Move to empty workspace        , movetoworkspace      , empty"

            "$mainMod SHIFT   , h           , Move left to workspace         , movewindow           , l"
            "$mainMod SHIFT   , l           , Move right to workspace        , movewindow           , r"
            "$mainMod SHIFT   , k           , Move up to workspace           , movewindow           , u"
            "$mainMod SHIFT   , j           , Move down to workspace         , movewindow           , d"

            "$mainMod CTRL + s, h           , Resize window left             , resizeactive         , -100 0"
            "$mainMod CTRL + s, l           , Resize window right            , resizeactive         , 100 0"
            "$mainMod CTRL + s, k           , Resize window up               , resizeactive         , 0 -100"
            "$mainMod CTRL + s, j           , Resize window down             , resizeactive         , 0 100"

            "$mainMod ALT     , H           , Move floating left             , moveactive           ,  -100 0"
            "$mainMod ALT     , L           , Move floating right            , moveactive           , 100 0"
            "$mainMod ALT     , K           , Move floating up               , moveactive           , 0 -100"
            "$mainMod ALT     , J           , Move floating down             , moveactive           , 0 100"

            "$mainMod         , Equal       , Volume down                    , exec                 , ${lib.getExe pkgs.pamixer} -i 2"
            "$mainMod         , Minus       , Volume up                      , exec                 , ${lib.getExe pkgs.pamixer} -d 2"

            "$mainMod         , mouse_down  , Scroll workspace               , workspace            , e-1"
            "$mainMod         , mouse_up    , Scroll workspace               , workspace            , e+1"
          ];

          # mouse binding
          bindm = [
            "$mainMod, mouse:272, movewindow"
            "$mainMod, mouse:273, resizewindow"
          ];

          windowrule = [
            "workspace 1      , title:Terminal"
            "workspace 2      , title:Web"
            "workspace 3      , title:Development"
            "workspace 4      , title:Chat"
            "workspace 8      , title:Steam"
            "workspace 10     , title:passwordManager"

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

            "noborder, onworkspace:w[t1]"

            "suppressevent maximize, class:.*"

            "workspace 2 silent, class:^(firefox)$"
            "workspace 2 silent, class:^(librewolf)$"

            "workspace 8 silent, class:^(Steam|steam|steam_app_.*)$, title:^((?!notificationtoasts.*).)*$"
            "workspace 8 silent, title:^(.*Steam[A-Za-z0-9\s]*)$"
            # "fullscreen, class:^(Steam|steam|steam_app_.*)$"
            # "fakefullscreen, class:^(Steam|steam|steam_app_.*)$"
            # "noinitialfocus, class:^(steam)$, title:^(notificationtoasts.*)$, floating:1"

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
          xrandr -r ${builtins.toString constants.refresh-rate}
          exec awesome
        '';
      };

      xdg.configFile."neovide/neovide.toml".source = (pkgs.formats.toml {}).generate "neovideExtraConfigDIY" {
        font = {
          normal = ["JetBrains Mono"];
          size = 13;
          # features = {
          #   "JetBrains Mono" = ["-liga"];
          # };
        };
      };

      xdg.configFile."zed/settings.json".text = builtins.toJSON {
        auto_update = false;
        base_keymap = "VSCode";
        theme = "Gruvbox Dark";
        vim_mode = true;
        ui_font_size = 16;
        buffer_font_size = 16;
        tab_size = 2;
        buffer_font_family = "JetBrains Mono";
        vim = {
          use_system_clipboard = "always";
        };
        BINDZ = [
          {
            context = "Editor && !VimWaiting && !menu";
            bindings = {
              ctrl-c = "editor::Copy"; # vim default: return to normal mode
              ctrl-v = "editor::Paste"; # vim default: visual block mode
              ctrl-y = "editor::Undo"; # vim default: line up
              ctrl-o = "workspace::Open"; # vim default: go back
              ctrl-a = "editor::SelectAll"; # vim default: increment
            };
          }
        ];
        inlay_hints = {
          enabled = true;
          show_type_hints = true;
          show_parameter_hints = true;
          show_other_hints = true;
          edit_debounce_ms = 700;
          scroll_debounce_ms = 50;
        };
        journal = {
          hour_format = "hour24";
        };
        telemetry = {
          diagnostics = false;
          metrics = false;
        };
      };
    };
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];

  system.stateVersion = "24.05"; # Dont change
}
