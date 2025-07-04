{
  ###### Imports ###### {{{
  lib,
  pkgs,
  self,
  config,
  inputs,
  conHome,
  options,
  conUsername,
  ...
}: {
  imports =
    [
      inputs.stylix.nixosModules.stylix
      inputs.home-manager.nixosModules.default
    ]
    ++ lib.my.withModules [
      "myConstants.nix"
      "myUnfree.nix"

      "neovim/myNeovim.nix"
      "myTerminal.nix"

      # "mySway.nix"
      # "myXMonad.nix"
      # "myAwesome.nix"
      "myHyprland.nix"

      "myZSH.nix"
      "myMPV.nix"
      "myTmux.nix"
      "myGaming.nix"

      "myVesktop.nix"
      "myDolphin.nix"
      "myLibrewolf.nix"
      "myComma.nix"

      "myPackages.nix"
      "myAudioEffects.nix"
      "mySyncthing.nix"
      "myMullvad.nix"
      "myStupid.nix"
      "myImpermanence.nix"
      "myTemplates.nix"
      "myXDGDirsEnforcement.nix"

      "visuals/myTheme.nix"
      "visuals/myCursor.nix"
      "visuals/myThemeCore.nix"
      "visuals/myKvantumBasedQT.nix"
    ];
  # }}}

  ###### Custom ###### {{{

  custom.defaultTerminal = pkgs.alacritty;

  # }}}

  ###### Essential or basic. ###### {{{

  services.dbus.implementation = "broker";

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

  # DNS
  networking.networkmanager.insertNameservers = ["9.9.9.9" "149.112.112.112"];

  networking.firewall.enable = true;

  time.timeZone = "Europe/Warsaw";

  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_NAME = "pl_PL.UTF-8";
      LC_TIME = "pl_PL.UTF-8";
      LC_CTYPE = "pl_PL.UTF-8";
      LC_PAPER = "pl_PL.UTF-8";
      LC_ADDRESS = "pl_PL.UTF-8";
      LC_COLLATE = "pl_PL.UTF-8";
      LC_NUMERIC = "pl_PL.UTF-8";
      LC_MONETARY = "pl_PL.UTF-8";
      LC_TELEPHONE = "pl_PL.UTF-8";
      LC_MEASUREMENT = "pl_PL.UTF-8";
      LC_IDENTIFICATION = "pl_PL.UTF-8";
    };
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
      eq10q

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
      bzip2
      p7zip
      unzip
      gnutar
      unrar-free
      atool # Unified CLI for all of these

      # Camera files support.
      gphoto2fs

      # CLI.
      eza
      deno
      bonk
      entr
      ncdu
      vlock
      pipenv
      rclone
      busybox
      exiftool
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
      gtrash # Cli trashcan
      file # File identifier
      zoxide # Cd alternative
      devenv # Dev environments
      libqalculate # Calculator
      udftools # Udf filesystem
      htop-vim # TUI task manager
      pulsemixer # Volume control
      ripgrep # Multithreaded grep
      xdg-utils # Includes xdg-open
      imagemagick # Image identifier
      ffmpeg # Video and magic editor
      gmic # Image processing language
      libnotify # Notifications (notify-send)
      python312Packages.ptpython # Python repl
      ntfs3g # ntfs filesystem interop (windows fs)

      # GUI.
      calibre
      foliate
      prismlauncher
      krita # Painting
      anki # Flashcards
      libreoffice # office
      nsxiv # Image viewer
      simplescreenrecorder
      godot_4 # Game engine
      sayonara # Music player
      inkscape # Vector graphics
      keepassxc # Password manager
      qbittorrent # Torrent client
      qalculate-gtk # Gui calculator
      resources # GUI task manager
      localsend # Send via local network
      xdragon # drag items from terminal
      tor-browser

      # Writing.
      typst
      asciidoctor
      pandoc # document converter

      # Haskell.
      stack
      cabal-install
      ghc # Haskell compiler for the LSP
      haskell-language-server # Haskell LSP
    ]
    ++ (
      if (config.const.importedMyMPVModule or false)
      then [python3]
      else []
    );

  # }}}

  ###### Miscellaneous ###### {{{

  programs.dconf.enable = true;
  xdg.menus.enable = true;
  xdg.icons.enable = true;

  # Create media folder in root
  systemd.tmpfiles.rules = [
    "d /media 0755 root root"
  ];

  # Envvar, envars. User ones go into home manager.
  environment.sessionVariables = {
    NH_FLAKE = config.const.flakePath; # For nix helper.
  };

  # This allows for programs to see audio plugins
  environment.variables = let
    homeConfig = config.home-manager.users.${conUsername};
    makePluginPath = format:
      (lib.makeSearchPath format [
        "$HOME/.nix-profile/lib"
        "/run/current-system/sw/lib"
        "/etc/profiles/per-user/$USER/lib"
      ])
      + ":${homeConfig.xdg.dataHome}/.${format}";
  in {
    DSSI_PATH = makePluginPath "dssi";
    LADSPA_PATH = makePluginPath "ladspa";
    LV2_PATH = makePluginPath "lv2";
    LXVST_PATH = makePluginPath "lxvst";
    VST_PATH = makePluginPath "vst";
    VST3_PATH = makePluginPath "vst3";
  };

  system.stateVersion = "24.05"; # Don't change.

  # }}}

  ##### NixOS ###### {{{

  # Keep trace of flake hash and flake for every gen in /etc
  system.extraSystemBuilderCmds = "ln -s ${self.sourceInfo.outPath} $out/src";
  environment.etc."flake-rev.json".text = builtins.toJSON {inherit (self) sourceInfo;};
  environment.etc."flake-src".source = lib.my.relativeToRoot ".";

  nixpkgs.config.allowUnfree = lib.mkForce false;

  nix = {
    channel.enable = false;
    settings = {
      warn-dirty = true;
      experimental-features = ["nix-command" "flakes"];
    };
    registry.nixpkgs.flake = self.inputs.nixpkgs;
    # registry.nixpkgs-unstable.flake = self.inputs.nixpkgs-unstable;
    nixPath = [
      "nixpkgs=${self.inputs.nixpkgs}"
      # "nixpkgs-unstable=${self.inputs.nixpkgs-unstable}"
    ];
  };
  programs.nix-ld.enable = true;
  ## If needed, you can add missing libraries here. nix-index-database is your friend to
  ## find the name of the package from the error message:
  ## https://github.com/nix-community/nix-index-database
  programs.nix-ld.libraries =
    options.programs.nix-ld.libraries.default;

  # }}}

  ###### Services ###### {{{

  # Printing.
  services.printing.enable = true;
  services.printing.cups-pdf.enable = true;

  # Needed for secrets.
  services.gnome.gnome-keyring.enable = true;

  # Automount.
  services.udisks2.enable = true;
  services.udev.extraRules = ''
    ENV{ID_FS_USAGE}=="filesystem|other|crypto", ENV{UDISKS_FILESYSTEM_SHARED}="1"
  '';

  services.libinput.enable = true;
  services.xserver.autoRepeatDelay = 170;
  services.xserver.autoRepeatInterval = 45;
  services.libinput.mouse.middleEmulation = false;
  services.libinput.mouse.accelProfile = "adaptive";
  # }}}

  ##### Home Manager ###### {{{
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {inherit inputs;};
    backupFileExtension = "backup"; # home-manager breaks without it.
    users.${conUsername} = {
      lib,
      config,
      osConfig,
      ...
    }: {
      imports = [
        inputs.nixvim.homeManagerModules.nixvim
        inputs.nix-index-database.hmModules.nix-index
      ];

      # Prevent default apps from being changed
      xdg.configFile."mimeapps.list".force = true;
      xdg.mimeApps = {
        enable = true;
        associations.added = config.xdg.mimeApps.defaultApplications;
        defaultApplications = let
          inherit (config.home.sessionVariables) EDITOR;
          inherit (config.home.sessionVariables) BROWSER;
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

          "image/bmp" = "nsxiv.desktop";
          "image/gif" = "nsxiv.desktop";
          "image/jpg" = "nsxiv.desktop";
          "image/jxl" = "nsxiv.desktop";
          "image/png" = "nsxiv.desktop";
          "image/avif" = "nsxiv.desktop";
          "image/heif" = "nsxiv.desktop";
          "image/jpeg" = "nsxiv.desktop";
          "image/tiff" = "nsxiv.desktop";
          "image/webp" = "nsxiv.desktop";
          "image/x-eps" = "nsxiv.desktop";
          "image/x-ico" = "nsxiv.desktop";
          "image/x-psd" = "nsxiv.desktop";
          "image/x-tga" = "nsxiv.desktop";
          "image/x-icns" = "nsxiv.desktop";
          "image/x-webp" = "nsxiv.desktop";
          "image/svg+xml" = "nsxiv.desktop";
          "image/x-xbitmap" = "nsxiv.desktop";
          "image/x-xpixmap" = "nsxiv.desktop";
          "image/x-portable-bitmap" = "nsxiv.desktop";
          "image/x-portable-pixmap" = "nsxiv.desktop";
          "image/x-portable-graymap" = "nsxiv.desktop";

          "image/vnd.djvu" = "org.pwmt.zathura.desktop";
          "application/pdf" = "org.pwmt.zathura.desktop";

          "video/dv" = "mpv.desktop";
          "video/3gp" = "mpv.desktop";
          "video/avi" = "mpv.desktop";
          "video/fli" = "mpv.desktop";
          "video/flv" = "mpv.desktop";
          "video/mp4" = "mpv.desktop";
          "video/ogg" = "mpv.desktop";
          "video/3gpp" = "mpv.desktop";
          "video/divx" = "mpv.desktop";
          "video/mp2t" = "mpv.desktop";
          "video/mpeg" = "mpv.desktop";
          "video/webm" = "mpv.desktop";
          "video/3gpp2" = "mpv.desktop";
          "video/x-avi" = "mpv.desktop";
          "video/x-flv" = "mpv.desktop";
          "video/x-m4v" = "mpv.desktop";
          "video/x-ogm" = "mpv.desktop";
          "video/mp4v-es" = "mpv.desktop";
          "video/msvideo" = "mpv.desktop";
          "video/x-mpeg2" = "mpv.desktop";
          "video/vnd.divx" = "mpv.desktop";
          "video/x-ms-asf" = "mpv.desktop";
          "video/x-ms-wmv" = "mpv.desktop";
          "video/x-ms-wmx" = "mpv.desktop";
          "video/x-theora" = "mpv.desktop";
          "video/quicktime" = "mpv.desktop";
          "video/x-msvideo" = "mpv.desktop";
          "video/x-ogm+ogg" = "mpv.desktop";
          "video/x-matroska" = "mpv.desktop";
          "video/vnd.mpegurl" = "mpv.desktop";
          "video/x-theora+ogg" = "mpv.desktop";
          "application/x-matroska" = "mpv.desktop";
          "video/vnd.rn-realvideo" = "mpv.desktop";

          "audio/aac" = "mpv.desktop";
          "audio/mp4" = "mpv.desktop";
          "audio/ogg" = "mpv.desktop";
          "audio/mpeg" = "mpv.desktop";
          "audio/x-mp3" = "mpv.desktop";
          "audio/x-wav" = "mpv.desktop";
          "audio/vorbis" = "mpv.desktop";
          "audio/x-flac" = "mpv.desktop";
          "audio/mpegurl" = "mpv.desktop";
          "audio/x-scpls" = "mpv.desktop";
          "audio/x-speex" = "mpv.desktop";
          "audio/x-ms-wma" = "mpv.desktop";
          "audio/x-vorbis" = "mpv.desktop";
          "audio/x-mpegurl" = "mpv.desktop";
          "audio/x-oggflac" = "mpv.desktop";
          "audio/x-musepack" = "mpv.desktop";
          "audio/x-vorbis+ogg" = "mpv.desktop";
          "audio/x-pn-realaudio" = "mpv.desktop";
          "audio/vnd.rn-realaudio" = "mpv.desktop";

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

      home = {
        username = "${conUsername}";
        homeDirectory = "${conHome}";
        stateVersion = "24.05"; # Don't change.

        shellAliases = {
          "ls" = "eza";
          "cp" = "cp -v";
          "rm" = "rm -I";
          "la" = "eza -a";
          "ll" = "eza -l";
          "qcalc" = "qalc";
          "lla" = "eza -la";
          "rt" = "gtrash put";
          "pkill" = "pkill -f";
          "countlines" = "tokei";
          "f" = ''cd "$(fzfcd)"'';
          "shutdown" = "poweroff";
          "lock" = "sudo vlock -nas";
          "grep" = "grep --color=auto";
          "cbonsai" = "cbonsai --screensaver";
          "pmem" = "vmrss"; # [p]rocess [mem]ory
          "date" = ''date +"%A, %d %B %Y, %H:%M:%S"'';
          "backup" = "sudo borgmatic --verbosity 1 --list --stats";
          "nhoffline" = "nh os switch -- --option substitute false";
          "listinstalledpackages" = "nix-store --query --requisites /run/current-system | cut -d- -f2- | sort -u";
          "record" = "arecord -t wav -r 48000 -c 1 -f S16_LE ${config.home.homeDirectory}/Pictures/audiocaptures/recording.wav";
          "search" = "sudo echo 'got sudo' && sudo find / -maxdepth 99999999 2>/dev/null | ${lib.getExe pkgs.fzf} -i -q $1";
        };

        sessionVariables = {
          # Default programs.
          PAGER = "moar";
          BROWSER = "librewolf";
          OPENER = "xdg-open";
          EDITOR = lib.mkDefault "vi";
          SHELL = lib.getExe pkgs.zsh;

          VISUAL = config.home.sessionVariables.EDITOR;
          SUDO_EDITOR = config.home.sessionVariables.EDITOR;

          # Unreal engine .net cli tool turn off telemetry.
          DOTNET_CLI_TELEMETRY_OPTOUT = "true";

          # Without this, games that use SDL will minimize when focus is lost
          SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS = 0;

          # Systemd is retarded and doesn't use normal pager variable :DDDDD
          SYSTEMD_PAGER = config.home.sessionVariables.PAGER;
        };
      };

      # Wayland, X, etc. support for session variables.
      systemd.user.sessionVariables = config.home.sessionVariables;

      # Development, internal.
      programs.zoxide.enable = true;
      programs.nix-index.enable = true;
      programs.home-manager.enable = true;
      programs.git-credential-oauth.enable = true;

      stylix.targets.zathura.enable = false;
      programs.zathura = let
        color = config.lib.stylix.colors.withHashtag;
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

          font = "${fonts.serif.name} ${builtins.toString fonts.sizes.terminal}";
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
        settings.global = {
          width = 300;
          height = 300;
          offset = "30x50";
          origin = "top-center";
        };
      };

      programs.lazygit = {
        enable = true;
        settings = {
          gui.border = "single";
          git = {
            overrideGpg = true;
            commit.signOff = true;
            branchLogCmd = "git log --graph --color=always --abbrev-commit --decorate --date=relative --pretty=medium --oneline {{branchName}} --";
          };
          customCommands = [
            {
              key = "f";
              command = "git difftool -y {{.SelectedLocalCommit.Sha}} -- {{.SelectedCommitFile.Name}}";
              context = "commitFiles";
              description = "Compare (difftool) with local copy";
            }
            {
              key = "F";
              command = "git pull --rebase --autostash {{ SelectedLocalBranch.Name }}";
              context = "localBranches";
              output = "log";
            }
          ];
        };
      };

      programs.tealdeer = {
        enable = true;
        settings = {
          updates.auto_update = true;
          display = {
            compact = false;
            use_pager = true;
          };
        };
      };

      programs.yazi = {
        enable = true;
        keymap = {
          mgr.prepend_keymap = [
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
        ignores = [
          ".git"
          ".cache"
          "devenv"
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
          ".local/share/Trash"
        ];
        extraOptions = [
          "--glob"
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
        aliases = {
          aa = "add -A"; # [A]dd [A]ll
          amend = "commit -a --amend";
          undo = "reset HEAD~1 --mixed";
          dlog = "-c diff.external=difft log --ext-diff";
          dshow = "-c diff.external=difft show --ext-diff";
          deleteGitignored = ''rm --cached ''${git ls-files -i -c --exclude-from=.gitignore}'';
          prettylog = "log --pretty=\"(%C(Green)%cr%C(reset)) %C(Cyan)%an: %C(reset)%s\" --date=short";
        };
        extraConfig = {
          init = {
            defaultBranch = "origin";
          };
          user = {
            signingKey = "86B6FCCC3563C00B";
            name = "saygo-png";
            email = "saygo.mail@proton.me";
          };
          color.ui = "auto";
          pull.rebase = true;
          commit.gpgsign = true;
          rerere.enabled = true;
          push.autoSetupRemote = true;
          branch.autosetupmerge = true;
          merge.tool = "${lib.getExe pkgs.meld}";
          core.excludesfile = "~/.gitignore_global";
          push = {
            default = "upstream";
            useForceIfIncludes = true;
          };
          diff = {
            tool = "vimdiff";
            mnemonicprefix = true;
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

      # rofi {{{
      programs.rofi = {
        package = pkgs.rofi-wayland;
        enable = true;
        extraConfig = {
          sort = true;
          padding = 10;
          scrollbar = true;
          show-match = true;
          matching = "fuzzy";
          show-icons = false;
          auto-select = false;
          run-command = "{cmd}";
          fixed-num-lines = true;
          sorting-method = "fzf";
          disable-history = false;
          modi = "window,run,drun";
          separator-style = "dash";
          drun-show-actions = false;
          window-format = "{w} {c}  {t}";
          font = let
            inherit (builtins) toString;
            inherit (config.stylix) fonts;
          in "${fonts.monospace.name} ${toString (fonts.sizes.terminal + 1)}";

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
          color = config.lib.stylix.colors.withHashtag;
        in
          lib.mkForce {
            "*" = {
              highlight = "bold";
              border-color = mkLiteral "#${osConfig.const.accentColor}";
              gruvbox-dark-fg0 = mkLiteral "${color.base06}";
              gruvbox-dark-fg1 = mkLiteral "${color.base05}";
              gruvbox-dark-gray = mkLiteral "${color.base04}";
              gruvbox-dark-red-dark = mkLiteral "${color.base09}";
              gruvbox-dark-red-light = mkLiteral "${color.base08}";
              gruvbox-dark-yellow-dark = mkLiteral "${color.base0A}";
              gruvbox-dark-yellow-light = mkLiteral "${color.base0C}";
              gruvbox-dark-bg0 = mkLiteral "rgba (40, 40, 40, 0%)";
              gruvbox-dark-bg3 = mkLiteral "rgba (125, 134, 24, 0%)";
              selected-normal-background = mkLiteral "#${osConfig.const.accentColor}";

              normal-background = mkLiteral "@background";
              gruvbox-dark-bg0-soft = mkLiteral "@background";
              alternate-normal-background = mkLiteral "@background";
              background-color = mkLiteral "@background";
              background = mkLiteral "@gruvbox-dark-bg0";
              foreground = mkLiteral "@gruvbox-dark-fg1";
              separatorcolor = mkLiteral "@border-color";
              active-foreground = mkLiteral "@foreground";
              normal-foreground = mkLiteral "@foreground";
              scrollbar-handle = mkLiteral "@border-color";
              urgent-foreground = mkLiteral "@gruvbox-dark-fg1";
              alternate-normal-foreground = mkLiteral "@foreground";
              urgent-background = mkLiteral "@gruvbox-dark-red-dark";
              active-background = mkLiteral "@gruvbox-dark-yellow-dark";
              selected-normal-foreground = mkLiteral "@gruvbox-dark-fg0";
              alternate-urgent-foreground = mkLiteral "@gruvbox-dark-fg1";
              selected-active-foreground = mkLiteral "@active-foreground";
              selected-urgent-foreground = mkLiteral "@urgent-foreground";
              alternate-active-background = mkLiteral "@active-background";
              alternate-active-foreground = mkLiteral "@active-foreground";
              alternate-urgent-background = mkLiteral "@urgent-background";
              selected-urgent-background = mkLiteral "@gruvbox-dark-red-light";
              selected-active-background = mkLiteral "@gruvbox-dark-yellow-light";
            };
            "window" = {
              padding = 5;
              border = 0;
              background-color = mkLiteral "@background";
            };
            "mainbox" = {
              border = 0;
              padding = 0;
            };
            "message" = {
              padding = mkLiteral "1px";
              border = mkLiteral "2px 0 0";
              border-color = mkLiteral "@background";
            };
            "textbox" = {
              highlight = mkLiteral "@highlight";
              text-color = mkLiteral "@foreground";
            };
            "listview" = {
              spacing = mkLiteral "0px";
              padding = mkLiteral "0px 0 0";
              border = mkLiteral "0px solid 0 0";
              border-color = mkLiteral "@background";
            };
            "element" = {
              border = 0;
              padding = mkLiteral "2px";
            };
            "inputbar" = {
              spacing = 2;
              padding = mkLiteral "2px";
              text-color = mkLiteral "@normal-foreground";
              children = mkLiteral "[prompt, textbox-prompt-sep, entry, case-indicator]";
            };
            "case-indicator, entry, prompt, button" = {
              spacing = 0;
              text-color = mkLiteral "@normal-foreground";
            };
            "case-indicator" = {
              spacing = 0;
              text-color = mkLiteral "@normal-foreground";
            };
            "entry" = {
              spacing = 0;
              text-color = mkLiteral "@normal-foreground";
            };
            "prompt" = {
              spacing = 0;
              text-color = mkLiteral "@normal-foreground";
            };
            "button" = {
              spacing = 0;
              text-color = mkLiteral "@normal-foreground";
            };
            "textbox-prompt-sep" = {
              str = ":";
              expand = false;
              margin = mkLiteral "0 0.2em 0.3em 0";
              text-color = mkLiteral "@normal-foreground";
            };
            "element.normal.normal" = {
              text-color = mkLiteral "@normal-foreground";
              background-color = mkLiteral "@normal-background";
            };
            "element.normal.urgent" = {
              text-color = mkLiteral "@urgent-foreground";
              background-color = mkLiteral "@urgent-background";
            };
            "element.normal.active" = {
              text-color = mkLiteral "@active-foreground";
              background-color = mkLiteral "@active-background";
            };
            "element.selected.normal" = {
              text-color = mkLiteral "@selected-normal-foreground";
              background-color = mkLiteral "@selected-normal-background";
            };
            "element.selected.urgent" = {
              text-color = mkLiteral "@selected-urgent-foreground";
              background-color = mkLiteral "@selected-urgent-background";
            };
            "element.selected.active" = {
              text-color = mkLiteral "@selected-active-foreground";
              background-color = mkLiteral "@selected-active-background";
            };
            "element.alternate.normal" = {
              text-color = mkLiteral "@alternate-normal-foreground";
              background-color = mkLiteral "@alternate-normal-background";
            };
            "element.alternate.urgent" = {
              text-color = mkLiteral "@alternate-urgent-foreground";
              background-color = mkLiteral "@alternate-urgent-background";
            };
            "element.alternate.active" = {
              text-color = mkLiteral "@alternate-active-foreground";
              background-color = mkLiteral "@alternate-active-background";
            };
            "scrollbar" = {
              border = 0;
              padding = 0;
              handle-width = mkLiteral "8px";
              width = mkLiteral "4px";
              handle-color = mkLiteral "@normal-background";
            };
            "mode-switcher" = {
              border = mkLiteral "2px 0 0";
              border-color = mkLiteral "@normal-background";
            };
            "button.selected" = {
              text-color = mkLiteral "@selected-normal-foreground";
              background-color = mkLiteral "@selected-normal-background";
            };
            "element-icon" = {
              text-color = mkLiteral "inherit";
              background-color = mkLiteral "inherit";
            };
            "element-text" = {
              text-color = mkLiteral "inherit";
              background-color = mkLiteral "inherit";
            };
          };
      };
      # }}}

      # Extra Configs {{{

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
        let-style = "inline";
        in-style = "left-align";
        comma-style = "trailing";
        haddock-style = "single-line";
      };

      xdg.configFile."stylish-haskell/config.yaml".text = ''
        steps:
          - simple_align:
              cases: always
              records: always
              multi_way_if: always
              top_level_patterns: always
          - imports:
              align: global
              list_padding: 4
              post_qualify: false
              group_imports: false
              separate_lists: true
              space_surround: false
              pad_module_names: true
              list_align: after_alias
              long_list_align: inline
              empty_list_align: inherit
              group_rules:
                - match: ".*"
                  sub_group: "^[^.]+"
          - language_pragmas:
              align: true
              style: vertical
              remove_redundant: true
              language_prefix: LANGUAGE
          - trailing_whitespace: {}
        cabal: true
        columns: 100
        newline: native
      '';
      # }}}
    };
  };
  # }}}
}
## vim:foldmethod=marker
