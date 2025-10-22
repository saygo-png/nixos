{
  ###### Imports ###### {{{
  lib,
  pkgs,
  self,
  config,
  inputs,
  conHome,
  options,
  nixvim-pkgs,
  pkgs-frozen,
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
      "packages.nix"
      "constants.nix"
      "templates.nix"
      "audioEffects.nix"
      "impermanence.nix"
      "xdgDirsEnforcement.nix"

      "mpv.nix"
      "git.nix"
      "iamb.nix"
      "rofi.nix"
      "tmux.nix"
      "qcalc.nix"
      "nixSearch.nix"
      "dolphin.nix"
      "mullvad.nix"
      "prismlauncher.nix"
      "vesktop.nix"
      "incus.nix"
      "librewolf.nix"
      "syncthing.nix"
      "neovim.nix"

      "visuals/theme.nix"
      "visuals/cursor.nix"
      "visuals/themeCore.nix"
      "visuals/kvantumBasedQt.nix"
    ];
  # }}}

  custom.defaultTerminal = {
    package = pkgs.alacritty;
    desktopFile = "alacritty.desktop";
  };

  ###### Essential or basic. ###### {{{

  services.dbus.implementation = "broker";
  services.speechd.enable = false; # Pullls in nearly a gig and is useless to me

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

  services.xserver.wacom.enable = true;

  environment.binsh = lib.getExe pkgs.dash;

  # Faster boot
  boot.initrd.systemd.network.wait-online.enable = false;
  networking.dhcpcd.wait = "background";

  networking.hostName = config.const.host;

  # Enable sysrq fully
  boot.kernel.sysctl."kernel.sysrq" = 1;

  # DNS
  networking.networkmanager.insertNameservers = ["9.9.9.9" "149.112.112.112"];

  networking.firewall.enable = true;

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
      symbolsFile = builtins.toFile "plfi" ''
                default partial alphanumeric_keys
                xkb_symbols "basic" {
                    include "latin"
                    name[Group1]="Polish-Fin";
        key <AE01> { [         1,     exclam,     notequal,   exclamdown ] };
                    key <AE02> { [         2,         at,  twosuperior, questiondown ] };
                    key <AE04> { [         4,     dollar,         cent,   onequarter ] };
                    key <AE05> { [         5,    percent,     EuroSign,        U2030 ] };
                    key <AE06> { [         6, asciicircum,     onehalf,   logicaland ] };
                    key <AE07> { [         7,  ampersand,      section,        U2248 ] };
                    key <AE08> { [         8,   asterisk, periodcentered, threequarters ] };
                    key <AE09> { [         9,  parenleft, guillemotleft,   plusminus ] };
                    key <AE10> { [         0, parenright, guillemotright,     degree ] };
                    key <AE11> { [     minus, underscore,       endash,       emdash ] };

                    key <AD01> { [         q,          Q,     Greek_pi,  Greek_OMEGA ] };
                    key <AD02> { [         w,          W,           oe,           OE ] };
                    key <AD03> { [         e,          E,      eogonek,      Eogonek ] };
                    key <AD04> { [         r,          R,    copyright,   registered ] };
                    key <AD05> { [         t,          T,       ssharp,    trademark ] };
                    key <AD08> { [         i,          I,   rightarrow,        U2194 ] };
                    key <AD09> { [         o,          O,        U00F6,        U00D6 ] };

                    key <AC01> { [         a,          A,        U00E4,        U00C4 ] };
                    key <AC02> { [         s,          S,       sacute,       Sacute ] };
                    key <AC04> { [         f,          F,           ae,           AE ] };
                    key <AC06> { [         h,          H, rightsinglequotemark, U2022 ] };
                    key <AC07> { [         j,          J,        schwa,        SCHWA ] };
                    key <AC08> { [         k,          K,     ellipsis,  dead_stroke ] };
                    key <TLDE> { [     grave, asciitilde,      notsign,    logicalor ] };

                    key <AB01> { [         z,          Z,    zabovedot,    Zabovedot ] };
                    key <AB02> { [         x,          X,       zacute,       Zacute ] };
                    key <AB03> { [         c,          C,       cacute,       Cacute ] };
                    key <AB04> { [         v,          V, doublelowquotemark, leftsinglequotemark ] };
                    key <AB05> { [         b,          B, rightdoublequotemark, leftdoublequotemark ] };
                    key <AB06> { [         n,          N,       nacute,       Nacute ] };
                    key <AB07> { [         m,          M,           mu,     infinity ] };
                    key <AB08> { [     comma,       less, lessthanequal,    multiply ] };
                    key <AB09> { [    period,    greater, greaterthanequal, division ] };

                    key <SPCE> { [     space,      space, nobreakspace, nobreakspace ] };

                    include "kpdl(comma)"

                    include "level3(ralt_switch)"
                };
      '';
      description = "Polish finnish layout";
    };
  };

  console = {
    useXkbConfig = true;
    font = "${pkgs.terminus_font}/share/consolefonts/ter-132n.psf.gz";
  };

  # Define a user account. Don't forget to set a password with ‘passwd $USERNAME’.
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

  # }}}

  ###### NixOS programs ###### {{{

  # Appimage support.
  programs.appimage = {
    enable = true;
    binfmt = true;
  };

  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.enableSSHSupport = true;

  # System packages.
  environment.systemPackages = with pkgs;
    [
      cups

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
      pkgs-frozen.eq10q

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
      deno
      bonk
      entr
      ncdu
      vlock
      rclone
      exiftool
      python3
      alsa-utils
      moar # Pager
      jq # Json parser
      termdown # Timer
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
      xdragon # drag items from terminal
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

  # }}}

  ###### Miscellaneous ###### {{{

  programs.dconf.enable = true;

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
    // {
      NH_FLAKE = config.const.flakePath; # For nix helper.
    };

  system.stateVersion = "25.05";

  # }}}

  ##### NixOS ###### {{{

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
      trusted-users = ["${conUsername}"];
      flake-registry = ""; # THIS IS HORRIBLE DEFAULT BEHAVIOUR
    };
    registry.nixpkgs.flake = inputs.nixpkgs;
    nixPath = ["nixpkgs=${inputs.nixpkgs}"];
  };

  programs.command-not-found.enable = false;
  programs.nix-index-database.comma.enable = true;
  programs.nix-index = {
    enableZshIntegration = false;
    enableFishIntegration = false;
    enableBashIntegration = false;
  };
  programs.nix-ld.enable = true;
  # If needed, you can add missing libraries here. nix-index-database is your friend to
  # find the name of the package from the error message:
  # https://github.com/nix-community/nix-index-database
  programs.nix-ld.libraries =
    options.programs.nix-ld.libraries.default;

  # }}}

  ###### Services ###### {{{

  # Needed for secrets.
  services.gnome.gnome-keyring.enable = true;

  # Automount.
  services.udisks2.enable = true;

  services.libinput.enable = true;
  services.xserver.autoRepeatDelay = 170;
  services.xserver.autoRepeatInterval = 45;
  services.libinput.mouse.middleEmulation = false;
  services.libinput.mouse.accelProfile = "adaptive";
  # }}}

  ##### Home Manager ###### {{{
  home-manager = let
    color = config.lib.stylix.colors.withHashtag;
  in {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {inherit inputs;};
    backupFileExtension = "backup"; # home-manager breaks without it.
    overwriteBackup = true;
    users.${conUsername} = {
      lib,
      config,
      ...
    }: {
      imports = [
        inputs.drugtracker2.homeManagerModules.default
      ];

      # Prevent default apps from being changed
      xdg = {
        configFile."mimeapps.list".force = true;
        mimeApps = {
          enable = true;
          associations.added = config.xdg.mimeApps.defaultApplications;
          defaultApplications = let
            inherit (config.home.sessionVariables) EDITOR;
            inherit (config.home.sessionVariables) BROWSER;
            fileBrowser = "org.kde.dolphin.desktop";
            imageViewer = "nsxiv.desktop";
            pdfViewer = "org.pwmt.zathura.desktop";
            videoPlayer = "mpv.desktop";
            terminal = "Alacritty.desktop";
          in {
            "text/plain" = "${EDITOR}.desktop";
            "text/rhtml" = "${EDITOR}.desktop";
            "text/x-tex" = "${EDITOR}.desktop";
            "text/x-java" = "${EDITOR}.desktop";
            "text/x-ruby" = "${EDITOR}.desktop";
            "text/x-cmake" = "${EDITOR}.desktop";
            "text/markdown" = "${EDITOR}.desktop";
            "text/x-python" = "${EDITOR}.desktop";
            "text/x-readme" = "${EDITOR}.desktop";
            "text/x-markdown" = "${EDITOR}.desktop";
            "application/json" = "${EDITOR}.desktop";
            "application/x-ruby" = "${EDITOR}.desktop";
            "application/x-yaml" = "${EDITOR}.desktop";
            "application/x-docbook+xml" = "${EDITOR}.desktop";
            "application/x-shellscript" = "${EDITOR}.desktop";

            "image/bmp" = imageViewer;
            "image/gif" = imageViewer;
            "image/jpg" = imageViewer;
            "image/jxl" = imageViewer;
            "image/png" = imageViewer;
            "image/avif" = imageViewer;
            "image/heif" = imageViewer;
            "image/jpeg" = imageViewer;
            "image/tiff" = imageViewer;
            "image/webp" = imageViewer;
            "image/x-eps" = imageViewer;
            "image/x-ico" = imageViewer;
            "image/x-psd" = imageViewer;
            "image/x-tga" = imageViewer;
            "image/x-icns" = imageViewer;
            "image/x-webp" = imageViewer;
            "image/svg+xml" = imageViewer;
            "image/x-xbitmap" = imageViewer;
            "image/x-xpixmap" = imageViewer;
            "image/x-portable-bitmap" = imageViewer;
            "image/x-portable-pixmap" = imageViewer;
            "image/x-portable-graymap" = imageViewer;

            "image/vnd.djvu" = pdfViewer;
            "application/pdf" = pdfViewer;

            "video/dv" = videoPlayer;
            "video/3gp" = videoPlayer;
            "video/avi" = videoPlayer;
            "video/fli" = videoPlayer;
            "video/flv" = videoPlayer;
            "video/mp4" = videoPlayer;
            "video/ogg" = videoPlayer;
            "video/3gpp" = videoPlayer;
            "video/divx" = videoPlayer;
            "video/mp2t" = videoPlayer;
            "video/mpeg" = videoPlayer;
            "video/webm" = videoPlayer;
            "video/3gpp2" = videoPlayer;
            "video/x-avi" = videoPlayer;
            "video/x-flv" = videoPlayer;
            "video/x-m4v" = videoPlayer;
            "video/x-ogm" = videoPlayer;
            "video/mp4v-es" = videoPlayer;
            "video/msvideo" = videoPlayer;
            "video/x-mpeg2" = videoPlayer;
            "video/vnd.divx" = videoPlayer;
            "video/x-ms-asf" = videoPlayer;
            "video/x-ms-wmv" = videoPlayer;
            "video/x-ms-wmx" = videoPlayer;
            "video/x-theora" = videoPlayer;
            "video/quicktime" = videoPlayer;
            "video/x-msvideo" = videoPlayer;
            "video/x-ogm+ogg" = videoPlayer;
            "video/x-matroska" = videoPlayer;
            "video/vnd.mpegurl" = videoPlayer;
            "video/x-theora+ogg" = videoPlayer;
            "application/x-matroska" = videoPlayer;
            "video/vnd.rn-realvideo" = videoPlayer;

            "audio/aac" = videoPlayer;
            "audio/mp4" = videoPlayer;
            "audio/ogg" = videoPlayer;
            "audio/mpeg" = videoPlayer;
            "audio/x-mp3" = videoPlayer;
            "audio/x-wav" = videoPlayer;
            "audio/vorbis" = videoPlayer;
            "audio/x-flac" = videoPlayer;
            "audio/mpegurl" = videoPlayer;
            "audio/x-scpls" = videoPlayer;
            "audio/x-speex" = videoPlayer;
            "audio/x-ms-wma" = videoPlayer;
            "audio/x-vorbis" = videoPlayer;
            "audio/x-mpegurl" = videoPlayer;
            "audio/x-oggflac" = videoPlayer;
            "audio/x-musepack" = videoPlayer;
            "audio/x-vorbis+ogg" = videoPlayer;
            "audio/x-pn-realaudio" = videoPlayer;
            "audio/vnd.rn-realaudio" = videoPlayer;

            "inode/directory" = fileBrowser;
            "terminal" = terminal;
            "text/html" = "${BROWSER}.desktop";
            "x-scheme-handler/ftp" = "${BROWSER}.desktop";
            "application/xhtml+xml" = "${BROWSER}.desktop";
            "x-scheme-handler/http" = "${BROWSER}.desktop";
            "x-scheme-handler/https" = "${BROWSER}.desktop";
            "x-scheme-handler/chrome" = "${BROWSER}.desktop";
            "application/x-extension-htm" = "${BROWSER}.desktop";
            "application/x-extension-xht" = "${BROWSER}.desktop";
            "application/x-extension-html" = "${BROWSER}.desktop";
            "application/x-extension-shtml" = "${BROWSER}.desktop";
            "application/x-extension-xhtml" = "${BROWSER}.desktop";
          };
        };
      };

      home = {
        username = "${conUsername}";
        homeDirectory = "${conHome}";
        stateVersion = "25.05";

        shellAliases = {
          "ls" = "eza";
          "cp" = "cp -v";
          "rm" = "rm -I";
          "la" = "eza -a";
          "ll" = "eza -l";
          "lla" = "eza -la";
          "pkill" = "pkill -f";
          "countlines" = "tokei";
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

        sessionVariables = rec {
          # Default programs.
          PAGER = "moar";
          BROWSER = "librewolf";
          OPENER = "xdg-open";
          EDITOR = lib.mkDefault "vi";
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
        sessionPath = ["${config.home.homeDirectory}/.local/bin"];
      };

      # Wayland, X, etc. support for session variables.
      systemd.user.sessionVariables = config.home.sessionVariables;

      # Development, internal.
      programs.zoxide.enable = true;
      programs.home-manager.enable = true;

      stylix.targets.zathura.enable = false;
      programs.zathura = let
        inherit (config.stylix) fonts;
      in {
        enable = true;
        options = {
          selection-clipboard = "clipboard";
          guioptions = "s";
          adjust-open = "width";
          statusbar-h-padding = 0;
          statusbar-v-padding = 0;
          scroll-page-aware = true;
          statusbar-home-tilde = true;

          font = "${fonts.serif.name} ${toString fonts.sizes.terminal}";
          recolor = false;
          recolor-keephue = false;

          notification-error-bg = "${color.base00}"; # bg
          notification-error-fg = "${color.base08}"; # bright:red
          notification-warning-bg = "${color.base00}"; # bg
          notification-warning-fg = "${color.base0A}"; # bright:yellow
          notification-bg = "${color.base00}"; # bg
          notification-fg = "${color.base0B}"; # bright:green

          completion-bg = "${color.base02}"; # bg2
          completion-fg = "${color.base05}"; # fg
          completion-group-bg = "${color.base01}"; # bg1
          completion-group-fg = "${color.base03}"; # gray
          completion-highlight-bg = "${color.base0B}"; # bright:blue
          completion-highlight-fg = "${color.base02}"; # bg2

          # Define the color in index mode
          index-bg = "${color.base02}"; # bg2
          index-fg = "${color.base05}"; # fg
          index-active-bg = "${color.base0B}"; # bright:blue
          index-active-fg = "${color.base02}"; # bg2

          inputbar-bg = "${color.base01}"; # bg
          inputbar-fg = "${color.base05}"; # fg

          statusbar-bg = "${color.base00}"; # bg2
          statusbar-fg = "${color.base05}"; # fg

          highlight-color = "${color.base0A}80"; # bright:yellow
          highlight-active-color = "${color.base09}80"; # bright:orange

          default-bg = "${color.base00}"; # bg
          default-fg = "${color.base05}"; # fg
          render-loading = true;
          render-loading-bg = "${color.base00}"; # bg
          render-loading-fg = "${color.base05}"; # fg

          # Recolor book content's color
          recolor-lightcolor = "${color.base00}"; # bg
          recolor-darkcolor = "${color.base05}"; # fg
        };
        extraConfig = ''
          unmap +
          unmap _
          map = zoom_in
          map - zoom_out
          map <C-j> zoom_in
          map <C-k> zoom_out

          map <C-r> reload
          map i toggle_statusbar;
        '';
      };

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
        keymap = {
          mgr.prepend_keymap = [
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
        };
        settings = {
          preview.tab_size = 2;
          mgr = {
            show_hidden = true;
            show_symlink = true;
            sort_by = "natural";
            sort_dir_first = true;
          };
          opener = {
            extract = [
              {
                run = ''aunpack "$@"'';
                desc = "Extract here";
              }
            ];
          };
          plugin = {
            # Disable image previews
            prepend_previewers = [
              {
                mime = "image/*";
                run = "noop";
              }
            ];
          };
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
            "break"
            "colors"
          ];
        };
      };

      programs.alacritty = {
        enable = true;
        settings = {
          scrolling.multiplier = 5;
          window = {
            dynamic_title = true;
            dynamic_padding = true;
            padding = lib.genAttrs ["x" "y"] (lib.const 4);
          };
          cursor = {
            style.blinking = "on";
            unfocused_hollow = false;
            style.shape = "Underline";
          };
          selection.save_to_clipboard = false;
          scrolling.history = 5000;
          keyboard.bindings = let
            mkBind = key: mods: action: {inherit key mods action;};
            mkBindC = key: mods: chars: {inherit key mods chars;};
          in [
            (mkBind "Escape" "Alt" "ToggleViMode")

            (mkBind "V" "Control" "Paste")
            (mkBind "C" "Control" "Copy")

            # Interrupt (ctrl + c)
            (mkBindC "C" "Control|Shift" "\\u0003")
          ];
        };
      };

      xdg.configFile."yapf/style" = {
        text = ''
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
      };

      xdg.configFile."fourmolu.yaml".source = (pkgs.formats.yaml {}).generate "fourmoluExtraConfigDIY" {
        indentation = 2;
        respectful = false;
        indent-wheres = true;
      };
    };
  };
  # }}}
}
## vim:foldmethod=marker

