{
  lib,
  pkgs,
  self,
  config,
  inputs,
  conHome,
  nixvim-pkgs,
  conUsername,
  ...
}: {
  imports =
    [
      inputs.stylix.nixosModules.stylix
      inputs.home-manager.nixosModules.default
      inputs.nix-index-database.nixosModules.nix-index
    ]
    ++ lib.my.withModules [
      "unfree.nix"
      "terminal.nix"
      "constants.nix"

      "zsh/zsh.nix"

      # "sway.nix"
      # "xmonad.nix"
      "hyprland.nix"
      "niri.nix"

      "gaming.nix"
      "stupid.nix"
      "zathura.nix"
      "mimeapps.nix"
      "packages.nix"
      "templates.nix"
      "audioEffects.nix"
      "impermanence.nix"
      "xdgDirsEnforcement.nix"

      "mpv.nix"
      "git.nix"
      "iamb.nix"
      "rofi.nix"
      "tmux.nix"
      "fish.nix"
      "qcalc.nix"
      "nixSearch.nix"
      "dolphin.nix"
      "mullvad.nix"
      "prismlauncher.nix"
      "vesktop.nix"
      "incus.nix"
      "alacritty.nix"
      "librewolf.nix"
      "syncthing.nix"
      "neovim.nix"

      "visuals/theme.nix"
      "visuals/cursor.nix"
      "visuals/themeCore.nix"
      "visuals/kvantumBasedQt.nix"
    ];

  custom.defaultTerminal = {
    package = pkgs.alacritty;
    desktopFile = "alacritty.desktop";
  };

  documentation = {
    dev.enable = true;
    info.enable = false;
    man = {
      generateCaches = true;
      man-db.enable = true;
    };
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Enable sysrq fully
  boot.kernel.sysctl."kernel.sysrq" = 1;

  # Faster boot
  boot.initrd.systemd.network.wait-online.enable = false;
  networking.dhcpcd.wait = "background";

  # Some programs ignore SIGTERM (notably "winedevice.exe") causing
  # a timeout until SIGKILL. This shortens this window.
  systemd.user.extraConfig = "DefaultTimeoutStopSec=10s";

  networking.hostName = config.const.host;

  # DNS
  networking.networkmanager.insertNameservers = ["9.9.9.9" "149.112.112.112"];

  networking.firewall.enable = true;

  environment.binsh = lib.getExe pkgs.dash;

  time.timeZone = "Europe/Warsaw";

  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = lib.genAttrs [
      "LC_NAME"
      "LC_TIME"
      "LC_CTYPE"
      "LC_PAPER"
      "LC_ADDRESS"
      "LC_COLLATE"
      "LC_NUMERIC"
      "LC_MONETARY"
      "LC_TELEPHONE"
      "LC_MEASUREMENT"
      "LC_IDENTIFICATION"
    ] (lib.const "pl_PL.UTF-8");
  };

  services.xserver.xkb = {
    layout = "pl,plfi";
    options = "caps:escape,grp:sclk_toggle";
    extraLayouts.plfi = {
      languages = ["pol"];
      symbolsFile =
        builtins.toFile "plfi"
        (builtins.readFile (lib.my.relativeToRoot "resources/polish-finnish-layout.xkb"));
      description = "Polish finnish layout";
    };
  };

  console = {
    useXkbConfig = true;
    font = "${pkgs.terminus_font}/share/consolefonts/ter-132n.psf.gz";
  };

  users = {
    mutableUsers = false;
    users = {
      ${conUsername} = {
        initialHashedPassword = "$y$j9T$vhbFbUi1uMXLH4qgYx13I1$dMmxiKFP4l236qd7ipfwmH.0PUnSehayI4jdRQmnzL0";
        hashedPasswordFile = "/persist/etc/password.hash";
        isNormalUser = true;
        extraGroups = ["wheel" "networkmanager" "video"]; # Enable ‘sudo’ for the user.
      };
      root = {
        initialHashedPassword = "$y$j9T$vhbFbUi1uMXLH4qgYx13I1$dMmxiKFP4l236qd7ipfwmH.0PUnSehayI4jdRQmnzL0";
        hashedPasswordFile = "/persist/etc/password.hash";
      };
    };
  };

  security.sudo.extraConfig = ''
    Defaults timestamp_timeout=-1
    Defaults passwd_timeout=0
    Defaults insults
  '';

  programs = {
    appimage = {
      enable = true;
      binfmt = true;
    };

    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

    dconf.enable = true;

    command-not-found.enable = false;
    nix-index-database.comma.enable = true;
    nix-index = {
      enableZshIntegration = false;
      enableFishIntegration = false;
      enableBashIntegration = false;
    };
  };

  # System packages.
  environment.systemPackages = with pkgs;
    [
      # Audio
      tap-plugins
      ( # Patch package so it doesn't spam menu entries
        pkgs.runCommand "lsp-plugins-no-xdg-menu" {}
        ''
          cp -R ${pkgs.lsp-plugins} $out
          chown -R $(id -u):$(id -g) $out/share/applications
          chmod -R 777 $out/share/applications
          rm -f $out/share/applications/*.desktop
        ''
      )
      zam-plugins
      airwindows-lv2
      molot-lite
      pkgs.eq10q

      # Nix.
      nh # Nix helper
      alejandra # Nix formatter
      nix-tree # Reverse dependency search
      nix-output-monitor # Pretty nix build output

      # All the archive garbage.
      xz
      zip
      gzip
      lzip
      p7zip
      unzip
      gnutar
      unrar-free
      atool # Unified CLI for all of these

      # CLI.
      eza
      vis
      bonk
      deno
      entr
      ncdu
      vlock
      rclone
      ddcutil
      python3
      exiftool
      hyperfine
      moreutils
      alsa-utils
      hydra-check
      moor # Pager
      jq # Json parser
      gcc # C compiling
      wget # Downloader
      tokei # Line counter
      udiskie # Auto mount
      cbonsai # pretty tree
      gnumake # C compiling
      file # File identifier
      zoxide # Cd alternative
      udftools # Udf filesystem
      htop-vim # TUI task manager
      pulsemixer # Volume control
      fourmolu # Haskell formatter
      ripgrep # Multithreaded grep
      xdg-utils # Includes xdg-open
      imagemagick # Image identifier
      libnotify # Notifications (notify-send)
      python312Packages.ptpython # Python repl
      ntfs3g # ntfs filesystem interop (windows fs)

      # GUI.
      foliate
      krita # Painting
      anki # Flashcards
      nsxiv # Image viewer
      simplescreenrecorder
      godot_4 # Game engine
      sayonara # Music player
      inkscape # Vector graphics
      keepassxc # Password manager
      qbittorrent # Torrent client
      mission-center # GUI task manager
      localsend # Send via local network
      tor-browser

      # Writing.
      typst
      asciidoctor
    ]
    ++ [
      # Haskell.
      nixvim-pkgs.stack
      nixvim-pkgs.cabal-install
      nixvim-pkgs.ghc # Haskell compiler for the LSP
    ];

  # Create media folder in root
  systemd.tmpfiles.rules = ["d /media 0755 root root"];

  # Envvar, envars. User ones go into home manager.
  environment.variables = let
    homeConfig = config.home-manager.users.${conUsername};
    makePluginPath = format:
      (lib.makeSearchPath format [
        "$HOME/.nix-profile/lib"
        "/run/current-system/sw/lib"
        "/etc/profiles/per-user/$USER/lib"
      ])
      + ":${homeConfig.xdg.dataHome}/.${format}";
  in
    lib.mapAttrs (_: makePluginPath) {
      # This allows for programs to see audio plugins
      LV2_PATH = "lv2";
      VST_PATH = "vst";
      DSSI_PATH = "dssi";
      VST3_PATH = "vst3";
      LXVST_PATH = "lxvst";
      LADSPA_PATH = "ladspa";
    }
    // rec {
      NH_FLAKE = config.const.flakePath; # For nix helper.
      # Default programs.
      PAGER = "moor";
      BROWSER = "librewolf";
      OPENER = "xdg-open";
      EDITOR = lib.mkDefault (lib.getExe' pkgs.vis "vis");
      SHELL = lib.getExe pkgs.zsh;

      VISUAL = EDITOR;
      SUDO_EDITOR = EDITOR;

      # Unreal engine .net cli tool turn off telemetry.
      DOTNET_CLI_TELEMETRY_OPTOUT = "true";

      # Without this, games that use SDL will minimize when focus is lost
      SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS = 0;

      # Systemd is retarded and doesn't use normal pager variable :DDDDD
      SYSTEMD_PAGER = PAGER;
    };

  system.stateVersion = "25.05";

  # Keep trace of flake hash and flake for every gen in /etc
  system.extraSystemBuilderCmds = "ln -s ${self.sourceInfo.outPath} $out/src";
  environment.etc."flake-rev.json".text = builtins.toJSON {inherit (self) sourceInfo;};
  environment.etc."flake-src".source = lib.my.relativeToRoot ".";

  nixpkgs.config.allowUnfree = lib.mkForce false;
  nixpkgs.config.allowBrokenPredicate = pkg: builtins.elem (lib.getName pkg) ["universum"];

  nix = {
    channel.enable = false;
    settings = {
      experimental-features = ["nix-command" "flakes"];
      download-buffer-size = 524288000; # 500mb
      trusted-users = lib.singleton "${conUsername}";
      flake-registry = ""; # THIS IS HORRIBLE DEFAULT BEHAVIOUR
    };
    registry.nixpkgs.flake = inputs.nixpkgs;
    nixPath = lib.singleton "nixpkgs=${inputs.nixpkgs}";
  };

  # programs.nix-ld.enable = true;
  # # If needed, you can add missing libraries here. nix-index-database is your friend to
  # # find the name of the package from the error message:
  # # https://github.com/nix-community/nix-index-database
  # programs.nix-ld.libraries =
  #   options.programs.nix-ld.libraries.default;

  # Needed for secrets.
  services.gnome.gnome-keyring.enable = true;

  # Automount.
  services.udisks2.enable = true;

  services.dbus.implementation = "broker";
  services.speechd.enable = false; # Pullls in nearly a gig and is useless to me

  services.libinput.enable = true;
  services.xserver.wacom.enable = true;
  services.xserver.autoRepeatDelay = 170;
  services.xserver.autoRepeatInterval = 45;
  services.libinput.mouse.middleEmulation = false;
  services.libinput.mouse.accelProfile = "adaptive";

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {inherit inputs;};
    backupFileExtension = "backup"; # home-manager breaks without it.
    overwriteBackup = true;
    users.${conUsername} = {
      lib,
      osConfig,
      config,
      ...
    }: {
      imports = [inputs.drugtracker2.homeManagerModules.default];

      home = {
        username = "${conUsername}";
        homeDirectory = "${conHome}";
        inherit (osConfig.system) stateVersion;
        sessionPath = lib.singleton "${config.home.homeDirectory}/.local/bin";
        shellAliases = {
          "ls" = "eza";
          "cp" = "cp -v";
          "rm" = "rm -I";
          "la" = "eza -a";
          "ll" = "eza -l";
          "lla" = "eza -la";
          "pkill" = "pkill -f";
          "f" = ''cd "$(fzfcd)"'';
          "shutdown" = "poweroff";
          "lock" = "sudo vlock -nas";
          "grep" = "grep --color=auto";
          "cbonsai" = "cbonsai --screensaver";
          "pmem" = "vmrss"; # [p]rocess [mem]ory
          "mdate" = ''date +"%A, %d %B %Y, %H:%M:%S"'';
          "cinst" = "cabal install --overwrite-policy=always";
          "backup" = "sudo borgmatic --verbosity 1 --list --stats";
          "nhoffline" = "nh os switch -- --option substitute false";
          "record" = "arecord -t wav -r 48000 -c 1 -f S16_LE ${config.home.homeDirectory}/Pictures/audiocaptures/recording.wav";
          "search" = "sudo echo 'got sudo' && sudo find / -maxdepth 99999999 2>/dev/null | ${lib.getExe pkgs.fzf} -i -q $1";
        };
      };

      programs.zoxide.enable = true;

      services.dunst = {
        enable = true;
        settings = {
          play_sound = {
            summary = "*";
            script = let
              ffplay = lib.getExe' pkgs.ffmpeg "ffplay";
              unsafe-soundFile = "${inputs.extras-nixos}/notifs/male-look-at-me.mp3";
              alert = pkgs.writeScriptBin "alert.dash" ''
                #!${lib.getExe pkgs.dash}
                ${ffplay} -v 0 -nodisp -autoexit ${lib.my.getSafePath unsafe-soundFile}
              '';
            in
              lib.getExe alert;
          };
          global = {
            width = 300;
            height = 300;
            offset = "30x50";
            origin = "top-center";
          };
        };
      };

      programs.tealdeer = {
        enable = true;
        enableAutoUpdates = true;
        settings.display = {
          compact = false;
          use_pager = true;
        };
      };

      programs.yazi = {
        enable = true;
        keymap.mgr.prepend_keymap = [
          {
            on = ["d"];
            run = ["remove --permanently"];
            desc = "remove permanently";
          }
          {
            on = ["b"];
            run = [
              # https://github.com/sxyazi/yazi/discussions/327
              ''shell '${pkgs.ripdrag}/bin/ripdrag "$@" -x -n 2>/dev/null &' --confirm''
            ];
            desc = "Drag and drop selection";
          }
        ];
        settings = {
          preview.tab_size = 2;
          mgr = {
            show_hidden = true;
            show_symlink = true;
            sort_by = "natural";
            sort_dir_first = true;
          };
          opener.extract = [
            {
              run = ''aunpack "$@"'';
              desc = "Extract here";
            }
          ];
          # Disable image previews
          plugin.prepend_previewers = [
            {
              mime = "image/*";
              run = "noop";
            }
          ];
        };
      };

      programs.fd = {
        enable = true;
        hidden = true;
        extraOptions = ["--glob"];
        ignores = [
          ".git"
          ".cache"
          "direnv"
          ".direnv"
          "lilipod"
          "nix/store"
          "nix/share"
          "libraries"
          "virtualenv"
          "virtualenvs"
          ".local/state"
          ".nix-profile"
          "nix/profiles"
          "node_modules"
          "cargo/registry"
        ];
      };

      programs.fzf = {
        enable = true;
        tmux.enableShellIntegration = true;

        defaultCommand = "fd --hidden --type f";
        defaultOptions = ["--no-height"];

        fileWidgetCommand = "fd --hidden --type f";
        fileWidgetOptions = ["--preview 'head {}'"];

        changeDirWidgetCommand = "fd --hidden --type d";
        changeDirWidgetOptions = ["--preview 'tree -C {} | head -200'"];

        # Transparent fzf.
        colors = {
          "bg" = lib.mkForce "-1";
          "bg+" = lib.mkForce "-1";
          "gutter" = lib.mkForce "-1";
        };
      };

      programs.drugtracker2 = {
        enable = true;
        columnString = " | ";
        rowString = "-";
        picker = pkgs.fzf;
        systemdIntegration.remindFrequency = "*:0/10:00";
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
        settings.modules = [
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
          "break"
          "colors"
        ];
      };

      xdg.configFile."yapf/style".text = ''
        [style]
          indent_width = 2
          use_tabs = False
          column_limit = 100
          based_on_style = google
          coalesce_brackets = True
          spaces_before_comment = 1
          join_multiple_lines = True
          continuation_indent_width = 2
          allow_multiline_lambdas = True
          dedent_closing_brackets = True
          continuation_align_style = SPACE
          spaces_around_power_operator = False
          split_before_bitwise_operator = True
          allow_multiline_dictionary_keys = True
          each_dict_entry_on_separate_line = False
          blank_line_before_class_docstring = False
          blank_lines_around_top_level_definition = 1
          blank_line_before_nested_class_or_def = False
          spaces_around_default_or_named_assign = False
          align_closing_bracket_with_visual_indent = True
          no_spaces_around_selected_binary_operators = *,/
          space_between_ending_comma_and_closing_bracket = False
      '';

      xdg.configFile."fourmolu.yaml".source = (pkgs.formats.yaml {}).generate "fourmoluConfig" {
        indentation = 2;
        respectful = false;
        indent-wheres = true;
      };
    };
  };
}
