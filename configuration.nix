{
  ###### Imports ###### {{{
  lib,
  host,
  pkgs,
  self,
  inputs,
  conHome,
  options,
  conUsername,
  conFlakePath,
  pkgs-unstable,
  conFlakePathRel,
  ...
}: {
  imports = [
    inputs.stylix.nixosModules.stylix
    inputs.home-manager.nixosModules.default
    "${conFlakePathRel}/modules/myZSHexperimental.nix"
    "${conFlakePathRel}/modules/myTmux.nix"
    "${conFlakePathRel}/modules/myRawQT.nix"
    "${conFlakePathRel}/modules/myNeovim.nix"
    "${conFlakePathRel}/modules/myAwesome.nix"
    "${conFlakePathRel}/modules/myHyprland.nix"
    "${conFlakePathRel}/modules/myPackages.nix"
  ];

  # }}}

  ###### Essential or basic. ###### {{{

  # This is needed for building, by default its set to 10% of ram, but that might not be enough for low ram systems and u will get an "out of space" error when trying to build. This will still happen with this option, since you need the resize first to even apply this config. So put this line in the vanilla config, rebuild, and then build my config.
  services.logind.extraConfig = "RuntimeDirectorySize=4G";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Faster boot
  boot.initrd.systemd.network.wait-online.enable = false;
  networking.dhcpcd.wait = "background";

  networking.hostName = "${host}";

  # DNS
  networking.nameservers = ["9.9.9.9" "149.112.112.112"];

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

  time.timeZone = "Europe/Warsaw";

  services.xserver.xkb = {
    extraLayouts = {
      plfi = {
        description = "Polish finnish layout";
        languages = ["pol"];
        symbolsFile = ./resources/static/symbols/plfi;
      };
    };
    layout = "pl,plfi";
    options = "caps:escape,grp:sclk_toggle";
  };

  console = {
    useXkbConfig = true;
    font = "${pkgs.terminus_font}/share/consolefonts/ter-132n.psf.gz";
  };

  # Define a user account. Don't forget to set a password with ‘passwd $USERNAME’.
  users.users.${conUsername} = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"]; # Enable ‘sudo’ for the user.
  };

  # Keep sudo password cached infinitely
  security.sudo.extraConfig = ''
    Defaults timestamp_timeout=-1
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

  # File manager.
  programs.thunar.enable = true;

  # Fixes dolphin not having mime types.
  environment.etc."/xdg/menus/applications.menu".text = builtins.readFile "${pkgs.kdePackages.plasma-workspace}/etc/xdg/menus/plasma-applications.menu";

  # System packages.
  environment.systemPackages = with pkgs; [
    # Nix.
    nh # Nix helper
    nil # Nix LSP
    nix-tree # Reverse dependency search
    nix-output-monitor # Pretty nix build output
    alejandra # Nix formatter

    # Other.
    firefox
    ncdu
    exiftool
    rclone
    ntfs3g # ntfs filesystem interop (windows fs)
    udftools # Udf filesystem
    wl-clipboard # Wayland xclip
    jq # Json parser
    nsxiv # Image viewer

    kdePackages.dolphin # File manager

    hydrus # Image collection
    patool # Universal archiver
    libqalculate # Calculator
    cups
    cups-bjnp
    # cups-brother-hl1110
    # cups-brother-hl2260d
    # cups-brother-hl1210w
    # cups-brother-hl3140cw
    # cups-brother-hll2375dw
    # cups-brother-hll2340dw
    # cups-brother-mfcl2750dw
    # cups-brother-hll3230cdw
    qalculate-gtk # ^
    vim # Text editor
    wget # Downloader
    trashy # Cli trashcan
    udiskie # Auto mount
    git # Source control
    fzf # Fuzzy finder
    file # File identifier
    imagemagick_light # Image identifier
    ripgrep # Multithreaded grep
    gnumake # C compiling
    xdg-utils # Includes xdg-open
    gcc # C compiling
    libnotify # Notifications (notify-send)

    # These are filepickers and whatnot
    xdg-desktop-portal-gtk
  ];

  # }}}

  ###### Miscellaneous ###### {{{

  # Most software has the HIP libraries hard-coded. You can work around it on NixOS by using:
  systemd.tmpfiles.rules = [
    "L+    /opt/rocm/hip   -    -    -     -    ${pkgs.rocmPackages.clr}"
    "d /media 0755 root root"
  ];

  # Envvar, envars. User ones go into home manager.
  environment.sessionVariables = {
    FLAKE = "${conFlakePath}"; # For nix helper.
  };

  systemd.user.services."wait-for-full-path-gtk" = {
    description = "wait for systemd units to have full PATH";
    wantedBy = ["xdg-desktop-portal-gtk.service"];
    before = ["xdg-desktop-portal-gtk.service"];
    path = with pkgs; [systemd coreutils gnugrep];
    script = ''
        ispresent () {
          systemctl --user show-environment | grep -E '^PATH=.*/.nix-profile/bin'
        }
      while ! ispresent; do
        sleep 0.1;
      done
    '';
    serviceConfig = {
      Type = "oneshot";
      TimeoutStartSec = "60";
    };
  };

  system.stateVersion = "24.05"; # Dont change # CHANGE IT ON UPDATE NO BALLS

  # }}}

  ##### NixOS ###### {{{
  system.extraSystemBuilderCmds = "ln -s ${self.sourceInfo.outPath} $out/src";
  nixpkgs.config.allowUnfree = false;
  nix.settings.experimental-features = ["nix-command" "flakes"];
  nix.settings.auto-optimise-store = true;
  nix.gc = {
    automatic = true;
    dates = "2day";
    options = "--delete-older-than 15d";
  };
  programs.nix-ld.enable = true;
  ## If needed, you can add missing libraries here. nix-index-database is your friend to
  ## find the name of the package from the error message:
  ## https://github.com/nix-community/nix-index-database
  programs.nix-ld.libraries =
    options.programs.nix-ld.libraries.default
    ++ (with pkgs; [
      gcc
      libgcc
      e2fsprogs
    ]);
  # }}}

  ###### Services ###### {{{

  # Printing
  services.printing.enable = true;
  services.printing.cups-pdf.enable = true;

  # Thumbnails for thunar
  services.tumbler.enable = true;

  # Automount
  services.udisks2.enable = true;
  services.udev.extraRules = ''
    ENV{ID_FS_USAGE}=="filesystem|other|crypto", ENV{UDISKS_FILESYSTEM_SHARED}="1"
  '';

  services.libinput.enable = true;
  services.libinput.mouse.accelProfile = "adaptive";
  services.libinput.mouse.middleEmulation = false;
  services.xserver.autoRepeatDelay = 170;
  services.xserver.autoRepeatInterval = 45;

  # File synchronization.
  services.syncthing = {
    enable = true;
    dataDir = conHome;
    user = conUsername;
    openDefaultPorts = true;
    overrideDevices = false;
    overrideFolders = false;
    settings.options.relaysEnabled = false;
    settings.devices = {
      nixos= {
        addresses = [
          "tcp://192.168.1.11:22000"
        ];
        id = "C6JMWDL-WYZZWGV-SJHKD5U-ZICMO7J-Z6L6T2T-LUHH3KH-TBIAGKK-FAO5TQF";
        autoAcceptFolders = true;
      };
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

  # }}}

  ###### Visuals ###### {{{

  # Fonts.
  fonts = {
    packages = with pkgs; [
      # Main font.
      courier-prime

      roboto
      noto-fonts
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
  stylix.cursor.size = 32;
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
    popups = lib.mkDefault 12;
    desktop = lib.mkDefault 12;
    terminal = lib.mkDefault 13;
    applications = lib.mkDefault 11;
  };

  # stylix.opacity = {
  #   popups = 0.7;
  #   desktop = 0.7;
  #   terminal = 0.7;
  #   applications = 0.5;
  # };

  stylix.opacity = {
    popups = 1.0;
    desktop = 1.0;
    terminal = 1.0;
    applications = 1.0;
  };

  xdg.portal.enable = true;
  xdg.portal.xdgOpenUsePortal = false;
  xdg.portal.extraPortals = [pkgs.xdg-desktop-portal-gtk];
  xdg.portal.config.common.default = "*";

  # Fixes some themeing/cursor issues and is needed for some things.

  programs.dconf.enable = true;

  xdg.menus.enable = true;
  # }}}

  ##### Home Manager ###### {{{
  home-manager = {
    extraSpecialArgs = {inherit inputs pkgs-unstable;};
    backupFileExtension = "backup"; # h-m breaks without it.
    users.${conUsername} = {
      lib,
      rasi,
      config,
      formats,
      # osConfig,
      ...
    }: {
      imports = [
        inputs.nix-index-database.hmModules.nix-index
        inputs.nixvim.homeManagerModules.nixvim
      ];

      # Needed for transparency.
      stylix.targets.fzf.enable = false;

      # Hopefully will set dark mode properly
      stylix.targets.gnome.enable = true;
      stylix.targets.kde.enable = true;

      dconf.settings = {
        # Remove min and max buttons
        "org/gnome/desktop/wm/preferences".button-layout = ":appmenu";
        # Prefer darkmode
        "org/gnome/desktop/interface".color-scheme = "prefer-dark";
      };

      # # Prevent default apps from being changed
      xdg.configFile."mimeapps.list".force = true;
      xdg.mimeApps = {
        enable = true;
        associations.added = config.xdg.mimeApps.defaultApplications;
        defaultApplications = {
          # Text
          "text/plain" = ["${config.home.sessionVariables.EDITOR}.desktop"];
          "text/rhtml" = ["${config.home.sessionVariables.EDITOR}.desktop"];
          "text/x-tex" = ["${config.home.sessionVariables.EDITOR}.desktop"];
          "text/x-java" = ["${config.home.sessionVariables.EDITOR}.desktop"];
          "text/x-ruby" = ["${config.home.sessionVariables.EDITOR}.desktop"];
          "inode/x-empty" = ["${config.home.sessionVariables.EDITOR}.desktop"];
          "text/x-python" = ["${config.home.sessionVariables.EDITOR}.desktop"];
          "text/x-readme" = ["${config.home.sessionVariables.EDITOR}.desktop"];
          "text/x-markdown" = ["${config.home.sessionVariables.EDITOR}.desktop"];
          "application/x-ruby" = ["${config.home.sessionVariables.EDITOR}.desktop"];
          "application/x-shellscript" = ["${config.home.sessionVariables.EDITOR}.desktop"];

          # Documents
          "image/svg+xml" = ["nsxiv.desktop"];
          "application/vnd.oasis.opendocument.text" = ["writer.desktop"];
          "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = ["writer.desktop"];

          # PDF
          "image/vnd.djvu" = ["org.pwmt.zathura.desktop"];
          "application/pdf" = ["org.pwmt.zathura.desktop"];

          # Web
          "text/html" = ["${config.home.sessionVariables.BROWSER}.desktop"];
          "x-scheme-handler/ftp" = ["${config.home.sessionVariables.BROWSER}.desktop"];
          "application/xhtml+xml" = ["${config.home.sessionVariables.BROWSER}.desktop"];
          "x-scheme-handler/http" = ["${config.home.sessionVariables.BROWSER}.desktop"];
          "x-scheme-handler/https" = ["${config.home.sessionVariables.BROWSER}.desktop"];
          "x-scheme-handler/chrome" = ["${config.home.sessionVariables.BROWSER}.desktop"];
          "application/x-extension-htm" = ["${config.home.sessionVariables.BROWSER}.desktop"];
          "application/x-extension-xht" = ["${config.home.sessionVariables.BROWSER}.desktop"];
          "application/x-extension-html" = ["${config.home.sessionVariables.BROWSER}.desktop"];
          "application/x-extension-shtml" = ["${config.home.sessionVariables.BROWSER}.desktop"];
          "application/x-extension-xhtml" = ["${config.home.sessionVariables.BROWSER}.desktop"];

          # Image
          "image/*" = ["nsxiv.desktop"];
          "image/png" = ["nsxiv.desktop"];
          "image/jpeg" = ["nsxiv.desktop"];
          "application/octet-stream" = ["nsxiv.desktop"]; #bmp

          # Video
          "video/ogg" = ["mpv.desktop"];
          "video/x-msvideo" = ["mpv.desktop"];
          "video/quicktime" = ["mpv.desktop"];
          "video/webm" = ["mpv.desktop"];
          "video/x-flv" = ["mpv.desktop"];
          "video/mp4" = ["mpv.desktop"];
          "application/x-flash-video" = ["mpv.desktop"];
          "video/MP2T" = ["mpv.desktop"];
          "image/x-tga" = ["mpv.desktop"];

          # Audio
          "audio/mpeg" = ["mpv.desktop"];
          "audio/x-flac" = ["mpv.desktop"];
          "audio/mp4" = ["mpv.desktop"];
          "application/ogg" = ["mpv.desktop"];
          "audio/x-mod" = ["mpv.desktop"];
        };
      };

      home = {
        username = "${conUsername}";
        homeDirectory = "${conHome}";
        stateVersion = "24.05"; # Dont change # CHANGE IT ON UPDATE NO BALLS

        pointerCursor.gtk.enable = true;

        shellAliases = {
          "f" = "fzfcd";
          "countlines" = "tokei";
          "cbonsai" = "cbonsai --screensaver";
          "backup" = "sudo borgmatic --verbosity 1 --list --stats";
          "date" = ''date +"%A, %d %B %Y, %H:%M:%S"'';
          "plan" = ''nsxiv ${conHome}/Sync/notes/plan.png'';
          "cp" = ''cp -v'';
          "more" = "${lib.getExe pkgs.moar}";
          "shutdown" = "poweroff";
          "pkill" = "pkill -f";
          "ls" = "${lib.getExe pkgs.eza}";
          "la" = "${lib.getExe pkgs.eza} -a";
          "ll" = "${lib.getExe pkgs.eza} -l";
          "rt" = "${lib.getExe pkgs.trashy}";
          "pmem" = "vmrss"; # [p]rocess [mem]ory
          "qcalc" = "${lib.getExe pkgs.libqalculate}";
          "grep" = "${lib.getExe pkgs.gnugrep} --color=auto";
          "nhoffline" = "nh os switch ${conFlakePath} -- --option substitute false";
          "search" = "sudo echo got sudo && sudo find / -maxdepth 99999999 2>/dev/null | ${lib.getExe pkgs.fzf} -i -q $1";
          "listinstalledpackages" = "nix-store --query --requisites /run/current-system | cut -d- -f2- | sort -u";
          "record" = "${lib.getExe' pkgs.alsa-utils "arecord"} -t wav -r 48000 -c 1 -f S16_LE ${conHome}/screencaptures/recording.wav";
        };

        sessionVariables = {
          # Default programs.
          PAGER = "moar";
          EDITOR = lib.mkDefault "vim";
          OPENER = "xdg-open";
          TERMINAL = "alacritty";
          BROWSER = "librewolf";
          VISUAL = config.home.sessionVariables.EDITOR;
          SUDO_EDITOR = config.home.sessionVariables.EDITOR;
          # Systemd is retarded and doesnt use normal pager variable :DDDDD
          SYSTEMD_PAGER = config.home.sessionVariables.PAGER;
          TERMINAL_PROG = config.home.sessionVariables.TERMINAL;
          XCURSOR_SIZE = config.home.pointerCursor.size;
          # Unreal engine .net cli tool turn off telemetry.
          DOTNET_CLI_TELEMETRY_OPTOUT = "true";
        };

        # Home packages, home manager packages, user packages, home programs
        packages = with pkgs; [
          # GUI.
          jetbrains.pycharm-community-src # python IDE
          libreoffice # office
          anki # Flashcards
          xdragon # drag items from terminal
          foliate # Ebook reader
          sayonara # Music player
          zathura # Better for pdfs
          fontforge-gtk # Font editor
          keepassxc # Password manager
          qbittorrent # Torrent client
          swappy # Quick drawing on images
          mission-center # GUI task manager
          localsend # Send via local network
          librewolf # Browser
          scribus

          # Dependencies for intersubs for mpv
          (pkgs.python3.withPackages (python312Packages: [
            python312Packages.pyqt5
            python312Packages.numpy
            python312Packages.six
            python312Packages.thttp
            python312Packages.beautifulsoup4
            python312Packages.requests
            python312Packages.lxml
            python312Packages.httpx
          ]))
          socat

          # Command line.
          cbonsai # pretty tree
          bc # Gnu calculator, needed for vmrss
          tokei # Line counter
          gmic # Image processing language
          moar # Pager
          termdown # Timer
          htop-vim # TUI task manager
          zoxide # Cd alternative
          pulsemixer # Volume control
          ffmpeg # Video and magic editor

          # Haskell
          haskell-language-server # Haskell LSP
          ghc # Haskell LSP

          # Unstable
          pkgs-unstable.krita # Painting
          pkgs-unstable.inkscape # Vector graphics

          # DO THIS ONCE LIBREWOLF GETS A HOME MANAGER MODULE TO MOVE .mozzila INTO CONFIG HOME
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
        ];

        activation.load-xresources =
          lib.hm.dag.entryAfter ["installPackages"] ''
          '';

        # This allows for semi-declarative configuration.
        # However it makes you lag when rebuilding.
        activation.configure-krita = lib.hm.dag.entryAfter ["writeBoundary"] ''
          run mkdir -p "${config.xdg.configHome}"

          run mkdir -p "${conHome}/.local/share/krita"
          run chmod -R $VERBOSE_ARG u+w,g+w "${conHome}/.local/share/krita"
          run cp -rf $VERBOSE_ARG "${builtins.toPath ./resources/krita/kritarc}" "${config.xdg.configHome}/kritarc"
          run cp -rf $VERBOSE_ARG "${builtins.toPath ./resources/krita/kritadisplayrc}" "${config.xdg.configHome}/kritadisplayrc"
          run cp -rf $VERBOSE_ARG "${builtins.toPath ./resources/krita/krita-toplevel}"/. "${conHome}/.local/share/krita"
          run chmod -R $VERBOSE_ARG u+w,g+w "${conHome}/.local/share/krita"

          run mkdir -p "${conHome}/.local/share/Anki2"
          run chmod -R $VERBOSE_ARG u+w,g+w "${conHome}/.local/share/Anki2"
          run cp -rf $VERBOSE_ARG "${builtins.toPath ./resources/anki}"/. "${conHome}/.local/share/Anki2"
          run chmod -R $VERBOSE_ARG u+w,g+w "${conHome}/.local/share/Anki2"
        '';

        activation.directories = lib.hm.dag.entryAfter ["writeBoundary"] ''
          run mkdir -p "${conHome}/Pictures/screenshots"
          run mkdir -p "${conHome}/backups"

          run mkdir -p "${conHome}/Desktop"
          run rm -f "${conHome}/Desktop/Desktop" || true

          run mkdir -p "${conHome}/screencaptures"
          run ln -s "${conHome}/screencaptures" "${conHome}/Desktop/screencaptures" || true
          run rm -f "${conHome}/screencaptures/screencaptures" || true

          run mkdir -p "${conHome}/Downloads"
          run ln -s "${conHome}/Downloads" "${conHome}/Desktop/Downloads" || true
          run rm -f "${conHome}/Downloads/Downloads" || true

          run mkdir -p "${conHome}/Videos"
          run ln -s "${conHome}/Videos" "${conHome}/Desktop/Videos" || true
          run rm -f "${conHome}/Videos/Videos" || true

          run mkdir -p "${conHome}/Sync"
          run ln -s "${conHome}/Sync" "${conHome}/Desktop/Sync" || true
          run rm -f "${conHome}/Sync/Sync" || true

          run mkdir -p "${config.xdg.configHome}"
          run ln -s "${config.xdg.configHome}" "${conHome}/Desktop/.config" || true
          run rm -f "${conHome}/Desktop/.config/.config" || true
          run rm -f "${conHome}/.config/.config" || true

          run mkdir -p "${config.xdg.dataHome}"
          run ln -s "${config.xdg.dataHome}" "${conHome}/Desktop/.local" || true
          run rm -f "${conHome}/Desktop/.local/.local" || true
          run rm -f "${conHome}/.local/.local" || true
        '';
      };

      # Wayland, X, etc. support for session vars
      systemd.user.sessionVariables = config.home.sessionVariables;

      # More visuals.
      gtk = {
        enable = true;
        gtk3.extraConfig.gtk-application-prefer-dark-theme = 1;
        gtk4.extraConfig.gtk-application-prefer-dark-theme = 1;
        iconTheme = {
          name = "Gruvbox-Plus-Dark";
          package = pkgs.gruvbox-plus-icons;
        };
      };

      # Development, internal.
      programs.bash.enable = true;
      programs.zoxide.enable = true;
      programs.nix-index.enable = true;
      programs.home-manager.enable = true;
      programs.git-credential-oauth.enable = true;
      programs.lazygit = {
        enable = true;
        settings.gui.border = "single";
      };

      programs.tealdeer = {
        enable = true;
        settings = {
          updates = {auto_update = true;};
          display = {
            compact = false;
            use_pager = true;
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
          "=" = "add sub-scale +0.1";
          "-" = "add sub-scale -0.1";
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
          watch-later-options-clr = true; # Dont save settings like brightness
        };
        scripts = [
          pkgs.mpvScripts.uosc
          pkgs.mpvScripts.acompressor
        ];
      };

      programs.yazi = {
        enable = true;
        settings.manager = {
          show_hidden = true;
          sort_dir_first = true;
          sort_by = "natural";
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
          "libraries"
          "virtualenv"
          "virtualenvs"
          "nix/store"
          "nix/share"
          ".local/state"
          ".nix-profile"
          "nix/profiles"
          "node_modules"
          "cargo/registry"
          ".local/share/Trash"
        ];
        extraOptions = [
          # "color=always"
          # "--follow"
          "--glob"
        ];
      };

      programs.fzf = {
        enable = true;
        defaultCommand = "fd --type f";
        defaultOptions = ["--no-height"];
        fileWidgetCommand = "fd --type f";
        changeDirWidgetCommand = "fd --type d";
        fileWidgetOptions = ["--preview 'head {}'"];
        changeDirWidgetOptions = ["--preview 'tree -C {} | head -200'"];
        colors = {
          "bg" = "-1";
          "bg+" = "-1";
          "gutter" = "-1";
        }; # Transparent fzf.
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
        userName = "saygo-png";
        package = pkgs.gitAndTools.gitFull;
        userEmail = "saygo.mail@proton.me";
        aliases = {
          aa = "add -A"; # [A]dd [A]ll
          amend = "commit -a --amend";
          undo = "reset HEAD~1 --mixed";
          deleteGitignored = "rm --cached `git ls-files -i -c --exclude-from=.gitignore`";
        };
        extraConfig = {
          pull = {rebase = true;};
          color = {ui = "auto";};
          merge = {tool = "splice";};
          rerere = {enabled = true;};
          push = {default = "simple";};
          branch = {autosetupmerge = true;};
          core = {excludesfile = "~/.gitignore_global";};
          diff = {
            tool = "vimdiff";
            mnemonicprefix = true;
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
          mouse = {hide-when-typing = "yes";};
          cursor = {blink = "yes";};
          scrollback = {lines = 1000;};
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
              mods = "Alt";
              action = "ToggleViMode";
            }
            # Copy paste
            {
              key = "V";
              mods = "Control";
              action = "Paste";
            }
            {
              key = "C";
              mods = "Control";
              action = "Copy";
            }
            {
              key = "C";
              mods = "Control|Shift";
              chars = "\x03";
            }
          ];
        };
      };

      # rofi {{{
      programs.rofi = {
        package = pkgs.rofi-wayland;
        enable = true;
        extraConfig = {
          modi = "window,run,drun";
          font = "${config.stylix.fonts.monospace.name} ${builtins.toString (config.stylix.fonts.sizes.terminal + 1)}";
          padding = 10;
          fixed-num-lines = true;
          show-icons = false;
          terminal = "${config.home.sessionVariables.TERMINAL}";
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
            gruvbox-dark-red-dark = lib.mkForce (mkLiteral "#fe8019");
            gruvbox-dark-red-light = lib.mkForce (mkLiteral "#fb4934");
            gruvbox-dark-yellow-dark = lib.mkForce (mkLiteral "#fabd2f");
            gruvbox-dark-yellow-light = lib.mkForce (mkLiteral "#8ec07c");
            gruvbox-dark-bg0 = lib.mkForce (mkLiteral "rgba (40, 40, 40, 0%)");
            gruvbox-dark-bg3 = lib.mkForce (mkLiteral "rgba (125, 134, 24, 0%)");
            selected-normal-background = lib.mkForce (mkLiteral "#7d8618");

            normal-background = lib.mkForce (mkLiteral "@background");
            gruvbox-dark-bg0-soft = lib.mkForce (mkLiteral "@background");
            alternate-normal-background = lib.mkForce (mkLiteral "@background");
            background-color = lib.mkForce (mkLiteral "@background");
            background = lib.mkForce (mkLiteral "@gruvbox-dark-bg0");
            foreground = lib.mkForce (mkLiteral "@gruvbox-dark-fg1");
            separatorcolor = lib.mkForce (mkLiteral "@border-color");
            active-foreground = lib.mkForce (mkLiteral "@foreground");
            normal-foreground = lib.mkForce (mkLiteral "@foreground");
            scrollbar-handle = lib.mkForce (mkLiteral "@border-color");
            urgent-foreground = lib.mkForce (mkLiteral "@gruvbox-dark-fg1");
            alternate-normal-foreground = lib.mkForce (mkLiteral "@foreground");
            urgent-background = lib.mkForce (mkLiteral "@gruvbox-dark-red-dark");
            active-background = lib.mkForce (mkLiteral "@gruvbox-dark-yellow-dark");
            selected-normal-foreground = lib.mkForce (mkLiteral "@gruvbox-dark-fg0");
            alternate-urgent-foreground = lib.mkForce (mkLiteral "@gruvbox-dark-fg1");
            selected-active-foreground = lib.mkForce (mkLiteral "@active-foreground");
            selected-urgent-foreground = lib.mkForce (mkLiteral "@urgent-foreground");
            alternate-active-background = lib.mkForce (mkLiteral "@active-background");
            alternate-active-foreground = lib.mkForce (mkLiteral "@active-foreground");
            alternate-urgent-background = lib.mkForce (mkLiteral "@urgent-background");
            selected-urgent-background = lib.mkForce (mkLiteral "@gruvbox-dark-red-light");
            selected-active-background = lib.mkForce (mkLiteral "@gruvbox-dark-yellow-light");
          };
          "window" = {
            padding = 5;
            border = 0;
            background-color = lib.mkForce (mkLiteral "@background");
          };
          "mainbox" = {
            border = 0;
            padding = 0;
          };
          "message" = {
            padding = mkLiteral "1px";
            border = mkLiteral "2px 0 0";
            border-color = lib.mkForce (mkLiteral "@background");
          };
          "textbox" = {
            highlight = mkLiteral "@highlight";
            text-color = lib.mkForce (mkLiteral "@foreground");
          };
          "listview" = {
            spacing = mkLiteral "0px";
            padding = mkLiteral "0px 0 0";
            border = mkLiteral "0px solid 0 0";
            border-color = lib.mkForce (mkLiteral "@background");
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
            background-color = lib.mkForce (mkLiteral "@normal-background");
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
            handle-color = lib.mkForce (mkLiteral "@normal-background");
          };

          "mode-switcher" = {
            border = mkLiteral "2px 0 0";
            border-color = lib.mkForce (mkLiteral "@normal-background");
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
        };
      };
      # }}}

      # Extra Configs {{{
      xdg.enable = true;

      # InterSubs plugin install
      xdg.configFile."mpv/scripts/" = {
        source = ./resources/mpv;
        recursive = true;
      };

      xdg.configFile."zathura/" = {
        text = ''
          set notification-error-bg       "${config.lib.stylix.colors.withHashtag.base00}" # bg
          set notification-error-fg       "${config.lib.stylix.colors.withHashtag.base08}" # bright:red
          set notification-warning-bg     "${config.lib.stylix.colors.withHashtag.base00}" # bg
          set notification-warning-fg     "${config.lib.stylix.colors.withHashtag.base0A}" # bright:yellow
          set notification-bg             "${config.lib.stylix.colors.withHashtag.base00}" # bg
          set notification-fg             "${config.lib.stylix.colors.withHashtag.base0B}" # bright:green

          set completion-bg               "${config.lib.stylix.colors.withHashtag.base02}" # bg2
          set completion-fg               "${config.lib.stylix.colors.withHashtag.base06}" # fg
          set completion-group-bg         "${config.lib.stylix.colors.withHashtag.base01}" # bg1
          set completion-group-fg         "${config.lib.stylix.colors.withHashtag.base03}" # gray
          set completion-highlight-bg     "${config.lib.stylix.colors.withHashtag.base0B}" # bright:blue
          set completion-highlight-fg     "${config.lib.stylix.colors.withHashtag.base02}" # bg2

          # Define the color in index mode
          set index-bg                    "${config.lib.stylix.colors.withHashtag.base02}" # bg2
          set index-fg                    "${config.lib.stylix.colors.withHashtag.base06}" # fg
          set index-active-bg             "${config.lib.stylix.colors.withHashtag.base0B}" # bright:blue
          set index-active-fg             "${config.lib.stylix.colors.withHashtag.base02}" # bg2

          set inputbar-bg                 "${config.lib.stylix.colors.withHashtag.base00}" # bg
          set inputbar-fg                 "${config.lib.stylix.colors.withHashtag.base06}" # fg

          set statusbar-bg                "${config.lib.stylix.colors.withHashtag.base02}" # bg2
          set statusbar-fg                "${config.lib.stylix.colors.withHashtag.base06}" # fg

          set highlight-color             "${config.lib.stylix.colors.withHashtag.base0A}" # bright:yellow
          set highlight-active-color      "${config.lib.stylix.colors.withHashtag.base09}" # bright:orange

          set default-bg                  "${config.lib.stylix.colors.withHashtag.base00}" # bg
          set default-fg                  "${config.lib.stylix.colors.withHashtag.base06}" # fg
          set render-loading              true
          set render-loading-bg           "${config.lib.stylix.colors.withHashtag.base00}" # bg
          set render-loading-fg           "${config.lib.stylix.colors.withHashtag.base06}" # fg

          # Recolor book content's color
          set recolor-lightcolor          "${config.lib.stylix.colors.withHashtag.base00}" # bg
          set recolor-darkcolor           "${config.lib.stylix.colors.withHashtag.base06}" # fg

          set recolor "true"
          set recolor-keephue false"

          set font "${config.stylix.fonts.serif.name} ${builtins.toString config.stylix.fonts.sizes.terminal}"

          map <C-r> reload
          map <C-j> zoom in
          map <C-k> zoom out

          unmap i
          map i toggle_statusbar

          set guioptions "s"
          set statusbar-home-tilde "true"
          set adjust-open "width"
          set statusbar-h-padding 0
          set statusbar-v-padding 0
          set scroll-page-aware "true"
          set selection-clipboard clipboard
        '';
      };

      xdg.configFile."wallpaper.png" = {
        source = ./resources/static/wallpaper.png;
      };

      xdg.configFile."yapf/style" = {
        text = ''
          [style]
            based_on_style = google
            indent_width = 2
            use_tabs = False
            column_limit = 100
            coalesce_brackets = True
            align_closing_bracket_with_visual_indent = True
            allow_multiline_dictionary_keys = True
            allow_multiline_lambdas = True
            blank_lines_around_top_level_definition = 1
            blank_line_before_class_docstring = False
            blank_line_before_nested_class_or_def = False
            continuation_align_style = SPACE
            continuation_indent_width = 2
            dedent_closing_brackets = True
            each_dict_entry_on_separate_line = False
            join_multiple_lines = True
            no_spaces_around_selected_binary_operators = *,/
            spaces_around_default_or_named_assign = False
            spaces_around_power_operator = False
            spaces_before_comment = 1
            space_between_ending_comma_and_closing_bracket = False
            split_before_bitwise_operator = True
            # Indent the dictionary value if it cannot fit on the same line as the
            # dictionary key. For example:
            #
            #   config = {
            #       'key1':
            #           'value1',
            #       'key2': value1 +
            #               value2,
            #   }
            #indent_dictionary_value = False
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
      # }}}
    };
  };
  # }}}
}
## vim:foldmethod=marker

