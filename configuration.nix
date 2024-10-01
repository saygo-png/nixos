{
  ###### Imports ###### {{{
  lib,
  host,
  pkgs,
  self,
  # config,
  inputs,
  conGaps,
  conHome,
  conUsername,
  conBorderSize,
  conFlakePath,
  conFlakePathRel,
  pkgs-unstable,
  conAccentColor,
  conRefresh-rate,
  # conScreen-width,
  # conScreen-height,
  ...
}: {
  imports = [
    inputs.stylix.nixosModules.stylix
    inputs.home-manager.nixosModules.default
    "${conFlakePathRel}/modules/myPackages.nix"
    "${conFlakePathRel}/modules/myTmux.nix"
  ];

  # }}}

  ###### Essential or basic. ###### {{{

  specialisation = {
    class.configuration = {
      home-manager.users.${conUsername} = {
        home.sessionVariables.BROWSER = lib.mkForce "firefox";
      };
    };
  };

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
    shell = pkgs.zsh;
  };

  # Keep sudo password cached infinitely
  security.sudo.extraConfig = ''
    Defaults timestamp_timeout=-1
  '';

  # }}}

  ###### NixOS programs ###### {{{

  programs.gnupg.agent = {
    enable = true;
  };
  programs.gnupg.agent.enableSSHSupport = true;

  # Needed here and in home manager.
  programs.hyprland = {
    enable = true;
  };

  # X11 window manager for games
  services.xserver.windowManager.awesome = {
    package = pkgs-unstable.awesome;
    enable = true;
  };

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
    appimage-run # Appimage runner

    # Other.
    firefox
    ncdu
    exiftool
    rclone
    ntfs3g # ntfs filesystem interop (windows fs)
    udftools # Udf filesystem
    xclip # Xorg wl-clipboard
    wl-clipboard # Wayland xclip
    jq # Json parser, needed for "hyprland-next-visible-client.sh"
    nsxiv # Image viewer

    kdePackages.dolphin # File manager

    # QT SHIT
    kdePackages.qtsvg # Icons for dolphin
    kdePackages.qtwayland # qt6
    libsForQt5.qt5.qtwayland

    # fix kirigami apps look
    # for example in filelight, without it the app looks weird
    # https://github.com/NixOS/nixpkgs/pull/202990#issuecomment-1328068486
    kdePackages.qqc2-desktop-style # qt6
    libsForQt5.qqc2-desktop-style

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
    xdg-desktop-portal-hyprland
    hyprland-protocols
  ];

  # }}}

  ###### Miscellaneous ###### {{{

  # Most software has the HIP libraries hard-coded. You can work around it on NixOS by using:
  systemd.tmpfiles.rules = [
    "L+    /opt/rocm/hip   -    -    -     -    ${pkgs.rocmPackages.clr}"
    "d /media 0755 root root"
  ];

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
  # }}}

  ###### Services ###### {{{

  # Enable the X11 windowing system.
  services.xserver.enable = true;

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

  # Might fix authorization agent issues
  # services.gnome.gnome-keyring.enable = true;

  # Polkit (needed for window managers)
  security.polkit.enable = true;

  # NixOS is retarded and turns on lightdm by default.
  services.displayManager.defaultSession = lib.mkDefault "none+awesome";
  services.xserver.displayManager = lib.mkDefault {
    lightdm.enable = false;
    startx.enable = true;
  };

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
      phone = {
        addresses = [
          "tcp://192.168.1.10:22000"
        ];
        id = "Z7AOC2O-CYXT6XV-Y67O5RB-VAXE2JT-JV36AMW-KWQ3U6Z-PVTINXB-IQ2UHQ7";
        autoAcceptFolders = false;
      };
      thinkpad = {
        addresses = [
          "tcp://192.168.1.13:22000 "
        ];
        id = "R3RAH4P-BEWWRO6-S5HYB2N-HZIHYCH-ERUDTE2-R2XLRAQ-CAZNG7U-S5BYYAF";
        autoAcceptFolders = false;
      };
    };
  };

  # Audio
  # RealtimeKit service, which hands out realtime scheduling priority to user processes on demand. For example, the PulseAudio server uses this to acquire realtime priority.
  security.rtkit.enable = true;

  # Pulseaudio.
  hardware.pulseaudio.enable = false;
  # Disable system-wide ALSA setup, since we're using PipeWire's ALSA emulation. Enabling this can
  # let us use media keys in TTY, for example.
  sound.enable = false;

  # Pipewire
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    jack.enable = true;
    pulse.enable = true;
    alsa.support32Bit = true;
    wireplumber.enable = true;
  };
  # }}}

  ###### Shell ###### {{{

  programs.zsh.enable = true;

  # Declare zsh as an available shell.
  environment.shells = [pkgs.zsh];

  # Provides autocompletion for system programs for zsh.
  environment.pathsToLink = ["/share/zsh"];

  # Envvar, envars. User ones go into home manager.
  environment.sessionVariables = {
    FLAKE = "${conFlakePath}"; # For nix helper.
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
  programs.dconf = {
    enable = true;
  };

  xdg.menus.enable = true;
  # }}}

  ##### Home Manager ###### {{{
  home-manager = {
    extraSpecialArgs = {inherit inputs pkgs-unstable;};
    backupFileExtension = "backup"; # h-m breaks without it.
    users.${conUsername} = {
      lib,
      config,
      osConfig,
      formats,
      rasi,
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

      # stylix.targets.regreet.enable = false;

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

        pointerCursor = {
          gtk.enable = true;
          x11.enable = true;
        };

        shellAliases = {
          "f" = "fzfcd";
          "countlines" = "tokei";
          "nix-shell" = "nix-shell --run zsh";
          "cbonsai" = "cbonsai --screensaver";
          "backup" = "sudo borgmatic --verbosity 1 --list --stats";
          "date" = ''date +"%A, %d %B %Y, %H:%M:%S"'';
          "plan" = ''nsxiv ${conHome}/Sync/notes/plan.png'';
          "cp" = ''cp -v'';
          "neov" = "neovide";
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
          EDITOR = "nvim";
          OPENER = "xdg-open";
          TERMINAL = "alacritty";
          BROWSER = "librewolf";

          # Fake running KDE
          # https://wiki.archlinux.org/title/qt#Configuration_of_Qt_5_applications_under_environments_other_than_KDE_Plasma
          # https://wiki.archlinux.org/title/Uniform_look_for_Qt_and_GTK_applications#The_KDE_Plasma_XDG_Desktop_Portal_is_not_being_used
          DESKTOP_SESSION = "KDE";

          VISUAL = config.home.sessionVariables.EDITOR;
          SUDO_EDITOR = config.home.sessionVariables.EDITOR;
          # Systemd is retarded and doesnt use normal pager variable :DDDDD
          SYSTEMD_PAGER = config.home.sessionVariables.PAGER;
          TERMINAL_PROG = config.home.sessionVariables.TERMINAL;
          XCURSOR_SIZE = config.home.pointerCursor.size;
          # Unreal engine .net cli tool turn off telemetry.
          DOTNET_CLI_TELEMETRY_OPTOUT = "true";
          QT_QPA_PLATFORM_PLUGIN_PATH = "${pkgs.libsForQt5.qt5.qtbase.bin}/lib/qt-${pkgs.libsForQt5.qt5.qtbase.version}/plugins/platforms";
        };

        # Home packages, home manager packages, user packages, home programs
        packages = with pkgs; [
          # GUI.
          jetbrains.pycharm-community-src # python IDE
          libreoffice # office
          anki # Flashcards
          xdragon # drag items from terminal
          neovide # Neovim gui
          foliate # Ebook reader
          sayonara # Music player
          zathura # Better for pdfs
          fontforge-gtk # Font editor
          keepassxc # Password manager
          qbittorrent # Torrent client
          flameshot # X11 screenshot tool
          swappy # Quick drawing on images
          mission-center # GUI task manager
          localsend # Send via local network
          hyprpicker # Color picker
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

        file = let
          # Qt config
          colorSchemeName = "gruvbox-medium-dark";
          mkScheme = colors: lib.concatStringsSep ", " (map (color: "#ff${color}") colors);
          colorScheme = lib.generators.toINI {} {
            ColorScheme = with config.lib.stylix.colors; {
              active_colors = mkScheme [
                base06 # Window text
                base00 # Button background
                base06 # Bright
                base05 # Less bright
                base01 # Dark
                base02 # Less dark
                base06 # Normal text
                base07 # Bright text
                base06 # Button text
                base00 # Normal background
                base00 # Window
                base00 # Shadow
                base02 # Highlight
                base05 # Highlighted text
                base0D # Link
                base0E # Visited link
                base00 # Alternate background
                base01 # Default
                base01 # Tooltip background
                base06 # Tooltip text
                base05 # Placeholder text
              ];

              inactive_colors = mkScheme [
                base04 # Window text
                base00 # Button background
                base05 # Bright
                base04 # Less bright
                base01 # Dark
                base02 # Less dark
                base04 # Normal text
                base05 # Bright text
                base04 # Button text
                base00 # Normal background
                base00 # Window
                base00 # Shadow
                base02 # Highlight
                base05 # Highlighted text
                base0D # Link
                base0E # Visited link
                base00 # Alternate background
                base01 # Default
                base01 # Tooltip background
                base05 # Tooltip text
                base04 # Placeholder text
              ];

              disabled_colors = mkScheme [
                base04 # Window text
                base00 # Button background
                base04 # Bright
                base03 # Less bright
                base00 # Dark
                base01 # Less dark
                base04 # Normal text
                base05 # Bright text
                base04 # Button text
                base00 # Normal background
                base00 # Window
                base00 # Shadow
                base02 # Highlight
                base05 # Highlighted text
                base0D # Link
                base0E # Visited link
                base00 # Alternate background
                base01 # Default
                base01 # Tooltip background
                base04 # Tooltip text
                base03 # Placeholder text
              ];
            };
          };

          baseConfig = {
            Appearance = {
              color_scheme_path = "${conHome}/.config/qt5ct/colors/${colorSchemeName}.conf";
              custom_palette = true;
              icon_theme = config.gtk.iconTheme.name;
              standard_dialogs = "default";
              # style = "Fusion";
              style = "Adwaita-Dark";
            };

            Troubleshooting = {
              force_raster_widgets = 1;
              ignored_applications = "@Invalid()";
            };

            Interface = {
              cursor_flash_time = 1200;
              double_click_interval = 400;
              menus_have_icons = true;
              show_shortcuts_in_context_menus = true;
              gui_effects = "@Invalid()";
              stylesheets = "@Invalid()";
              buttonbox_layout = 3; # GNOME dialog button layout
              toolbutton_style = 4; # Follow the application style
              activate_item_on_single_click = 1; # ... - i think that means let the application decide
              dialog_buttons_have_icons = 1; # ...
              underline_shortcut = 1; # ...
              wheel_scroll_lines = 3;
              keyboard_scheme = 2; # X11
            };
          };
        in {
          "bin/hyprfullscreenfix".source = ./resources/static/hyprfullscreenfix;
          # auto xrdb
          ${config.xresources.path}.onChange = ''
            [[ -z "$\{DISPLAY:-}" ]] && echo Display not set && exit 0
            run ${lib.getExe pkgs.xorg.xrdb} "${config.xresources.path}"
          '';

          ".xinitrc" = {
            text = ''
              if test -z "$DBUS_SESSION_BUS_ADDRESS"; then
                eval $(dbus-launch --exit-with-session --sh-syntax)
              fi
              systemctl --user import-environment DISPLAY XAUTHORITY XDG_CURRENT_DESKTOP
              if command -v dbus-update-activation-environment >/dev/null 2>&1; then
                dbus-update-activation-environment DISPLAY XAUTHORITY XDG_CURRENT_DESKTOP
              fi
              systemctl --user import-environment PATH &
              dbus-update-activation-environment --systemd PATH &
              hash dbus-update-activation-environment 2>/dev/null &

              export XDG_SESSION_TYPE=x11
              xrandr -r ${builtins.toString conRefresh-rate}
              ${lib.getExe' pkgs.polkit-kde-agent "polkit-kde-authentication-agent-1"} &
              ${lib.getExe pkgs.xmousepasteblock} &
              ${lib.getExe pkgs.xssproxy} &
              udiskie &
              $TERMINAL &
              exec dbus-run-session awesome
            '';
          };

          # QT theme
          ".config/qt5ct/colors/${colorSchemeName}.conf".text = colorScheme;
          ".config/qt6ct/colors/${colorSchemeName}.conf".text = colorScheme;
          ".config/qt5ct/qt5ct.conf".text = lib.generators.toINI {} (
            baseConfig
            // {
              Fonts = {
                fixed = "\"${config.stylix.fonts.monospace.name},${toString config.stylix.fonts.sizes.applications},-1,5,50,0,0,0,0,0,Regular\"";
                general = "\"${config.stylix.fonts.sansSerif.name},${toString config.stylix.fonts.sizes.applications},-1,5,50,0,0,0,0,0,Regular\"";
              };
            }
          );
          ".config/qt6ct/qt6ct.conf".text = lib.generators.toINI {} (
            baseConfig
            // {
              Fonts = {
                fixed = "\"${config.stylix.fonts.monospace.name},${toString config.stylix.fonts.sizes.applications},-1,5,400,0,0,0,0,0,0,0,0,0,0,1,Regular\"";
                general = "\"${config.stylix.fonts.sansSerif.name},${toString config.stylix.fonts.sizes.applications},-1,5,400,0,0,0,0,0,0,0,0,0,0,1,Regular\"";
              };
            }
          );
        };

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

      qt = {
        enable = true;
        platformTheme.name = "qtct";
        style.package = with pkgs; [
          adwaita-qt
          adwaita-qt6
        ];
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
        enableZshIntegration = true;
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

      programs.zsh = {
        enable = true;
        history.save = 50;
        history.size = 50;
        defaultKeymap = "viins";
        enableCompletion = false;
        autosuggestion.enable = true;
        syntaxHighlighting.enable = true;
        historySubstringSearch.enable = true;
        syntaxHighlighting.highlighters = ["brackets"];
        initExtra = builtins.readFile ./resources/zsh-extraConfig;
        plugins = [
          {
            name = "vi-mode";
            src = pkgs.zsh-vi-mode;
            file = "share/zsh-vi-mode/zsh-vi-mode.plugin.zsh";
          }
        ];
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

      # nixvim, neovim, vim, nvim # {{{
      programs.nixvim = {
        enable = true;
        extraPackages = with pkgs; [
          typos-lsp
          vale
          jq # Json formatter
          vim-language-server
          deadnix # Nix linter
          pyright # python lsp
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
          clojure-lsp # Clojure lsp
          cljfmt # Clojure formatter
          clj-kondo # Clojure linter
          zprint # Clojure formatter
          sumneko-lua-language-server
          isort # Python import sorter
          prettierd # Javascript formatter
          nodePackages.bash-language-server
          markdownlint-cli # Markdown linter
          stylish-haskell # Haskell formatter
          haskell-language-server # Haskell lsp
          vscode-langservers-extracted # Web LSPs
          python312Packages.mccabe # Flake8 plugin
          python312Packages.pyflakes # Python linter
          haskellPackages.fourmolu # Haskell formatter
          luajitPackages.jsregexp # Needed for luasnip
          nodePackages.prettier # Javascript formatter
          python312Packages.jedi # Autocomplete plugin
          python312Packages.pyls-isort # Python import sort
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
          linebreak = true;
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
          foldenable = false;
          foldmethod = "marker";

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
          maplocalleader = ",";

          rainbow_active = 1;

          gruvbox_material_foreground = "original";
          gruvbox_material_enable_bold = 0;
          gruvbox_material_transparent_background = 2;

          # Neovide neovim gui client.
          neovide_transparency = config.stylix.opacity.terminal;
          neovide_transparency_point = 0;
          neovide_background_color = "${config.lib.stylix.colors.withHashtag.base00}";
          neovide_padding_top = lib.mkDefault 8;
          neovide_padding_bottom = lib.mkDefault 0;
          neovide_padding_right = lib.mkDefault 6;
          neovide_padding_left = lib.mkDefault 6;
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
          "ftplugin/markdown.vim" = ''
            setlocal wrap
          '';
        };

        extraConfigVim = builtins.readFile ./resources/nvim-extraConfig.vim;
        extraConfigLuaPre = ''
          -- Hide deprecation warnings, i used this as a fix to
          -- multicursors plugin, but might be useful later on
          -- local notify = vim.notify
          -- vim.notify = function(msg, ...)
          --   if msg:match("has been deprecated") then
          --     return
          --   end
          --   notify(msg, ...)
          -- end
        '';
        extraConfigLuaPost = ''
          -- Makes treesitter work with rainbow plugin
          vim.api.nvim_set_hl(0, "@constructor", { link = "" })
          -- vim.api.nvim_set_hl(0, "@constructor.lua", { link = "" })
          vim.api.nvim_set_hl(0, "@punctuation.bracket", { link = "" })
          vim.api.nvim_set_hl(0, "@punctuation.special", { link = "" })
          vim.api.nvim_set_hl(0, "@punctuation.delimiter", { link = "" })
          vim.api.nvim_set_hl(0, "@variable.parameter.haskell", { link = "" })
        '';

        extraConfigLua = ''
          -- Vim as terminal
          vim.cmd[[
            augroup neovim_terminal
                autocmd!
                " Enter Terminal-mode (insert) automatically
                autocmd TermOpen * startinsert
                " Disables number lines on terminal buffers
                autocmd TermOpen * :setlocal nonumber norelativenumber laststatus=0
            augroup END

            " Vim visual multi binds
            let g:VM_leader = '\'
            let g:VM_maps = {}
            let g:VM_maps["Add Cursor Down"] = '<M-j>'
            let g:VM_maps["Add Cursor Up"] = '<M-k>'
          ]]

          -- Neovide
          if vim.g.neovide then
            vim.cmd[[colorscheme gruvbox-material]]
            vim.o.background = 'dark'
            vim.o.guifont = '${config.stylix.fonts.monospace.name}:h${builtins.toString (config.stylix.fonts.sizes.terminal + 1)}:#e-antialias:#h-slight'
            vim.cmd [[ hi Normal guibg=${config.lib.stylix.colors.withHashtag.base00} ]]
          end

          local open_command = "xdg-open"
          if vim.fn.has("mac") == 1 then
            open_command = 'open'
          end

          local function url_repo()
            local cursorword = vim.fn.expand('<cfile>')
            if string.find(cursorword, '^[a-zA-Z0-9-_.]*/[a-zA-Z0-9-_.]*$') then
              cursorword = "https://github.com/" .. cursorword
            end
            return cursorword or ""
          end

          vim.keymap.set('n', 'gx', function()
            vim.fn.jobstart({ open_command, url_repo() }, { detach = true })
          end, { silent = true })

          -- Keymaps
          -- Keep selection when indenting.
          vim.keymap.set("v", ">", ">gv", { desc = "Keep selection after indenting" })
          vim.keymap.set("v", "<", "<gv", { desc = "Keep selection after unindenting" })

          -- Keep cursor position after yank
          vim.keymap.set("n", "y", "ygv<esc>", { desc = "Keep cursor position after yank" })

          -- Window switching.
          vim.keymap.set("n", "<C-h>", ":wincmd h<CR>", { desc = "Move to the split on the left side" })
          vim.keymap.set("n", "<C-l>", ":wincmd l<CR>", { desc = "Move to the split on the right side" })
          vim.keymap.set("n", "<C-k>", ":wincmd k<CR>", { desc = "Move to the split above" })
          vim.keymap.set("n", "<C-j>", ":wincmd j<CR>", { desc = "Move to the split below" })

          -- Previous buffer
          vim.keymap.set('n', '<S-B>', '<C-6>')

          -- Conflicts with lsp hover
          vim.g["conjure#mapping#doc_word"] = false

          -- Split movement
          vim.keymap.set("n", "<S-M-h>", "<cmd>wincmd h<CR>", { desc = "Move to the split on the left side" })
          vim.keymap.set("n", "<S-M-l>", "<cmd>wincmd l<CR>", { desc = "Move to the split on the right side" })
          vim.keymap.set("n", "<S-M-k>", "<cmd>wincmd k<CR>", { desc = "Move to the split above" })
          vim.keymap.set("n", "<S-M-j>", "<cmd>wincmd j<CR>", { desc = "Move to the split below" })
          -- In nvim terminal
          vim.keymap.set("t", "<S-M-h>", "<c-\\><c-n><c-w>h", { desc = "Move to the split on the left side" })
          vim.keymap.set("t", "<S-M-l>", "<c-\\><c-n><c-w>j", { desc = "Move to the split on the right side" })
          vim.keymap.set("t", "<S-M-k>", "<c-\\><c-n><c-w>k", { desc = "Move to the split above" })
          vim.keymap.set("t", "<S-M-j>", "<c-\\><c-n><c-w>l", { desc = "Move to the split below" })
          -- Shift + Esc for normal mode in nvim terminal
          vim.keymap.set("t", "<S-M-Esc>", "<C-\\><C-n>", { desc = "Normal mode in terminal mode" })
          vim.keymap.set("t", "<S-M-Esc>", "<C-\\><C-n>", { desc = "Normal mode in terminal mode" })

          -- Plugins
          local utils = require "telescope.utils"
          local builtin = require "telescope.builtin"

          vim.keymap.set("n", "<leader>f", "<cmd>Oil<CR>", {desc = "[f]ile browser"})
          vim.keymap.set("n", "K", "<cmd>Lspsaga hover_doc<CR>", {desc = "hover"})
          vim.keymap.set("n", "<leader>a", "<cmd>Lspsaga code_action<CR>", {desc = "code [a]ctions"})
          vim.keymap.set("n", "<leader>th", "<cmd>Telescope harpoon marks<CR>", { silent = true, desc = "[t]elescope [h]arpoon Marks" })
          vim.keymap.set("n", "<leader>tb", builtin.current_buffer_fuzzy_find, { desc = "[t]elescope [b]uffer" })
          vim.keymap.set("n", "<leader>tn", builtin.help_tags, { desc = "[t]elescope [n]oob" })
          vim.keymap.set("n", "<leader>tk", builtin.keymaps, { desc = "[t]elescope [k]eymaps" })
          vim.keymap.set("n", "<leader>tf", builtin.find_files, { desc = "[t]elescope [f]iles" })
          vim.keymap.set("n", "<leader>ts", builtin.builtin, { desc = "[t]elescope [s]elect telescope" })
          vim.keymap.set("n", "<leader>tw", builtin.grep_string, { desc = "[t]elescope current [w]ord" })
          vim.keymap.set("n", "<leader>tg", builtin.live_grep, { desc = "[t]elescope by [g]rep" })
          vim.keymap.set("n", "<leader>td", builtin.diagnostics, { desc = "[t]elescope [d]iagnostics" })
          vim.keymap.set("n", "<leader>tr", builtin.resume, { desc = "[t]elescope [r]esume" })
          vim.keymap.set("n", "<leader>t.", builtin.oldfiles, { desc = "[t]elescope recent files (. for repeat)" })
          vim.keymap.set("n", "<leader><leader>", builtin.buffers, { desc = "[ ] Find existing buffers" })
          -- dial.nvim
          local augend = require("dial.augend")
          require("dial.config").augends:register_group{
            default = {
              augend.constant.alias.alpha,
              augend.constant.alias.Alpha,
              augend.constant.alias.bool,
              augend.date.alias["%-d.%-m."],
              augend.date.alias["%d.%m."],
              augend.date.alias["%d/%m/%y"],
              augend.date.alias["%d/%m/%Y"],
              augend.date.alias["%H:%M"],
              augend.date.alias["%H:%M:%S"],
              augend.date.alias["%-m/%-d"],
              augend.date.alias["%m/%d"],
              augend.date.alias["%m/%d/%y"],
              augend.date.alias["%m/%d/%Y"],
              augend.date.alias["%Y/%m/%d"],
              augend.integer.alias.binary,
              augend.integer.alias.decimal,
              augend.integer.alias.decimal_int,
              augend.integer.alias.hex,
              augend.integer.alias.octal,
              augend.semver.alias.semver,
            },
            typescript = {
              augend.constant.new{ elements = {"let", "const"} },
            },
          }
          vim.keymap.set("n", "<C-a>",  function() require("dial.map").manipulate("increment", "normal")  end)
          vim.keymap.set("n", "<C-x>",  function() require("dial.map").manipulate("decrement", "normal")  end)
          vim.keymap.set("n", "g<C-a>", function() require("dial.map").manipulate("increment", "gnormal") end)
          vim.keymap.set("n", "g<C-x>", function() require("dial.map").manipulate("decrement", "gnormal") end)
          vim.keymap.set("v", "<C-a>",  function() require("dial.map").manipulate("increment", "visual")  end)
          vim.keymap.set("v", "<C-x>",  function() require("dial.map").manipulate("decrement", "visual")  end)
          vim.keymap.set("v", "g<C-a>", function() require("dial.map").manipulate("increment", "gvisual") end)
          vim.keymap.set("v", "g<C-x>", function() require("dial.map").manipulate("decrement", "gvisual") end)

          vim.keymap.set("n", "<leader>rn", function()
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

          vim.keymap.set("n", "<Leader>c", function()
          require("conform").format({ timeout_ms = 500 })
          end, { desc = "[c]onform" })

          vim.keymap.set("n", "<leader>tcf", function()
            builtin.find_files({ cwd = utils.buffer_dir() })
          end, { silent = true, desc = "[t]elescope find [f]iles in [c]urrent buffer" })

          vim.keymap.set("n", "<leader>tcg", function()
            builtin.live_grep({ cwd = utils.buffer_dir() })
          end, { silent = true, desc = "[t]elescope grep in [c]urrent buffer" })

          -- Clipboard
          vim.keymap.set("n", "<c-v>", '"+p', { desc = "proper paste" })
          vim.keymap.set({"i", "c"}, "<C-V>", "<C-r>+", { desc = "Proper paste" })

          -- Basic
          vim.keymap.set("n", ";", ":", { desc = "Command mode with or without shift"})
          vim.keymap.set("n", ";", ":", { desc = "Command mode with or without shift"})
          vim.keymap.set("n", ";", ":", { desc = "Command mode with or without shift"})
          vim.keymap.set("n", ">", ">>", { desc = "Indent more", silent = true })
          vim.keymap.set("n", "<lt>", "<lt><lt>", { desc = "Indent less", silent = true })
          vim.keymap.set("v", ".", "<cmd>normal .<CR>", { desc = "Dot commands over visual blocks" })
          vim.keymap.set("n", "G", "Gzz", { desc = "Center bottom" })
          vim.keymap.set("n", "gg", "ggzz", { desc = "Center top" })
          vim.keymap.set("n", "gm", "m", { desc = "Set mark" })
          vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")
          vim.keymap.set("v", "gj", "J", { desc = "join lines" })
          vim.keymap.set("v", "J", ":m '>+1<CR>gv==kgvo<esc>=kgvo", { desc = "move highlighted text down" })
          vim.keymap.set("v", "K", ":m '<-2<CR>gv==jgvo<esc>=jgvo", { desc = "move highlighted text up" })
          vim.keymap.set( "i", "<C-r>", "<C-r><C-o>", { desc = "Insert contents of named register. Inserts text literally, not as if you typed it." })

          -- Autocomplete
          vim.keymap.set("i", "<C-x>", "<C-x><C-o>", { desc = "Autocomplete" })

          vim.keymap.set('n', '<leader>q', vim.cmd.quit)
          vim.keymap.set('n', '<leader>Q', vim.cmd.only)

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
            }
          })
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
          pkgs.vimPlugins.dial-nvim
          pkgs.vimPlugins.vim-dispatch
          pkgs.vimPlugins.vim-jack-in
        ];

        keymaps = [
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
        ];

        # Plugins {{{
        plugins = {
          nix.enable = false;
          flash.enable = true;
          comment.enable = true;
          fugitive.enable = true;
          surround.enable = true;
          friendly-snippets.enable = true;

          # Lisps
          conjure.enable = false;
          parinfer-rust.enable = false;

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
                "1" = "<C-h>";
                "2" = "<C-j>";
                "3" = "<C-k>";
                "4" = "<C-l>";
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
          };

          lspsaga = {
            enable = true;
            lightbulb = {
              enable = false;
              sign = false;
              virtualText = false;
            };
            symbolInWinbar.enable = false;
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
            nixvimInjections = true;
            nixGrammars = true; # Install grammars with Nix
            ensureInstalled = ["all"];
            ignoreInstall = ["comment"];
            incrementalSelection = {
              enable = true;
              keymaps = {
                initSelection = "<Enter>";
                nodeIncremental = "<Enter>";
                scopeIncremental = "grc";
                nodeDecremental = "<BS>";
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
            # Disable highlights from LSP
            # onAttach = ''
            #   client.server_capabilities.semanticTokensProvider = nil
            # '';
            servers = {
              # Nix.
              nil-ls = {
                enable = true;
                settings.nix.flake.autoArchive = true;
              };

              # Python.
              pyright.enable = true;

              # Bash
              bashls.enable = true;

              # Typos.
              typos-lsp = {
                enable = true;
                extraOptions.init_options.diagnosticSeverity = "Hint";
              };

              # Lua.
              lua-ls.enable = true;

              # Clojure
              clojure-lsp.enable = true;

              # Typescript
              tsserver.enable = true;
              eslint.enable = true; # Linter as lsp

              # Markdown
              marksman.enable = true;

              # Haskell.
              hls.enable = true;

              rust-analyzer = {
                enable = true;
                installCargo = false;
                installRustc = false;
              };
            };
            keymaps.lspBuf = {
              "gd" = "definition";
              "gD" = "references";
              "gi" = "implementation";
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
              lsp_fallback = false;
            };
            formattersByFt = {
              # Conform will run multiple formatters sequentially.
              json = ["jq"];
              sh = ["shfmt"];
              lua = ["stylua"];
              nix = ["alejandra"];
              clojure = ["zprint"];
              haskell = ["fourmolu"];
              python = ["isort" "yapf"];
              javascript = ["prettierd"];
              typescript = ["prettierd"];
              javascriptreact = ["prettierd"];
              typescriptreact = ["prettierd"];
              css = ["prettierd"];
              html = ["prettierd"];
              graphql = ["prettierd"];
              markdown = ["prettierd"];
              # Use the "*" filetype to run formatters on all filetypes.
              "*" = ["trim_whitespace"];
            };
            formatters = {
              cljfmt = {
                command = "${lib.getExe pkgs.cljfmt}";
                args = ["fix" "-"];
                stdin = true;
              };
            };
          };

          lint = {
            enable = true;
            lintersByFt = {
              rst = ["vale"];
              text = ["vale"];
              c = ["clangtidy"];
              cpp = ["clangtidy"];
              haskell = ["hlint"];
              json = ["jsonlint"];
              markdown = ["markdownlint"];
              bash = ["shellcheck"];
              shell = ["shellcheck"];
              clojure = ["clj-kondo"];
              nix = ["nix" "deadnix"];
              dockerfile = ["hadolint"];
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
              "<Leader>t" = "+[t]elescope";
              "<Leader>h" = "+[h]arpoon";
              "<leader>ha" = "[h]arpoon [a]dd file";
              "<leader>hm" = "[h]arpoon [m]enu";
              "<leader>hcm" = "[h]arpoon [c]ommand [m]enu";
              "<leader>hn" = "[h]arpoon [n]ext";
              "<leader>hp" = "[h]arpoon [p]revious";
              "<C-h>" = "harpoon file 1";
              "<C-j>" = "harpoon file 2";
              "<C-k>" = "harpoon file 3";
              "<C-l>" = "harpoon file 4";
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

          cmp = {
            enable = true;
            autoEnableSources = true;
            settings = {
              autocomplete = true;
              performance = {
                debounce = 200;
                throttle = 200;
                maxViewEntries = 5;
                fetchingTimeout = 50;
              };
              snippet.expand = ''
                function(args)
                  require('luasnip').lsp_expand(args.body)
                end
              '';
              sources = [
                {name = "nvim_lsp";}
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

        # }}}
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
      # }}}

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

      # hyprland {{{
      services.hyprpaper.enable = lib.mkForce false; # Enabled by default with hyprland.
      wayland.windowManager.hyprland = let
        gaps_in = conGaps;
        gaps_out = conGaps * 2;
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
            env = QT_QPA_PLATFORMTHEME, qt5ct
            env = QT_WAYLAND_DISABLE_WINDOWDECORATION, 1
            env = QT_AUTO_SCREEN_SCALE_FACTOR, 1
            env = MOZ_ENABLE_WAYLAND, 1
            env = GTK_USE_PORTAL, 1
          '';

        settings = {
          debug.disable_logs = true;
          xwayland = {
            enabled = true;
            force_zero_scaling = true;
          };
          # Autostart.
          exec-once = [
            "${lib.getExe' pkgs.polkit-kde-agent "polkit-kde-authentication-agent-1"} &"
            "${lib.getExe pkgs.swaybg} -m fill -i ${config.stylix.image} &"
            "systemctl --user import-environment PATH &"
            "hash dbus-update-activation-environment 2>/dev/null &"
            "dbus-update-activation-environment --systemd --all &"
            "udiskie &"
            "hyprctl dispatch exec '[workspace 2 silent] $BROWSER' &"
            "hyprctl dispatch exec '[workspace 1 silent] $TERMINAL' &"
          ];

          input = {
            kb_layout = osConfig.services.xserver.xkb.layout;
            kb_options = osConfig.services.xserver.xkb.options;
            repeat_delay = osConfig.services.xserver.autoRepeatDelay;
            repeat_rate = osConfig.services.xserver.autoRepeatInterval;
            accel_profile = osConfig.services.libinput.mouse.accelProfile;
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
            border_size = conBorderSize;
            border_part_of_window = false;
            no_border_on_floating = false;
            "col.active_border" = lib.mkForce "rgba(${conAccentColor}FF)";
            "col.inactive_border" = lib.mkForce "rgba(${config.stylix.base16Scheme.base00}00)";
          };

          group = {
            "col.border_active" = lib.mkForce "rgba(${conAccentColor}FF)";
            "col.border_inactive" = lib.mkForce "rgba(${conAccentColor}00)";
            "col.border_locked_active" = lib.mkForce "rgba(${config.stylix.base16Scheme.base0C}FF)";
            "col.border_locked_inactive" = lib.mkForce "rgba(${config.stylix.base16Scheme.base0C}00)";
            groupbar = {
              gradients = false;
              "col.active" = lib.mkForce "rgba(${conAccentColor}FF)";
              "col.inactive" = lib.mkForce "rgba(${conAccentColor}00)";
              "col.locked_active" = lib.mkForce "rgba(${config.stylix.base16Scheme.base0C}FF)";
              "col.locked_inactive" = lib.mkForce "rgba(${config.stylix.base16Scheme.base0C}00)";
            };
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
            blur = {
              enabled = false;
              size = 8;
              passes = 2;
              new_optimizations = true;
              special = true;
            };
            drop_shadow = true;
            dim_around = 0.8;
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

            "$mainMod, p, Toggle float, setfloating,"
            "$mainMod, p, Toggle [p]in, pin,"
            "$mainMod, p, Toggle [p]in, tagwindow, 69PINNED69"

            "$mainMod, v, Toggle float, togglefloating,"
            "$mainMod, v, Toggle [p]in, tagwindow, -69PINNED69"

            "$mainMod, f, [f]ullscreen, exec, hyprfullscreenfix"
            "$mainMod SHIFT, f, [f]ake fullscreen, fakefullscreen"

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
            "$mainMod SHIFT, r, [r]ecord area, exec, hyprcorder.sh -a"

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

            "$mainMod, Equal, Volume down, exec, ${lib.getExe pkgs.pamixer} -i 2 && notify-send -t 500 $(${lib.getExe pkgs.pamixer} --get-volume-human)"
            "$mainMod, Minus, Volume up, exec, ${lib.getExe pkgs.pamixer} -d 2 && notify-send -t 500 $(${lib.getExe pkgs.pamixer} --get-volume-human)"
          ];

          # mouse binding
          bindm = [
            "$mainMod, mouse:272, movewindow"
            "$mainMod, mouse:273, resizewindow"
          ];

          layerrule = [
            "blur, rofi"
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

            "noblur,^(?!(rofi))"

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

            "tile             , neovide"

            "idleinhibit focus, mpv"

            "float            ,udiskie"
          ];

          windowrulev2 = [
            # Needed for gloss window to tile and not focus.
            "tile, class:^()$"
            "noinitialfocus, class:^()$"

            # Drag and drop hack fixes.
            "nofocus, class:^krita$, title:^Krita$, floating:1"
            "nofocus, class:^Inkscape$, title:^Inkscape$, floating:1"
            "nofocus, class:^dolphin$, title:^Dolphin$, floating:1"

            # Shadow only for floating windows.
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
      # }}}

      # Extra Configs {{{
      xdg.enable = true;

      xdg.configFile."awesome/" = {
        source = ./resources/awesome;
        recursive = true;
      };

      xdg.configFile."awesome/theme.lua" = {
        text = ''
          local theme_assets = require("beautiful.theme_assets")
          local xresources = require("beautiful.xresources")
          local dpi = xresources.apply_dpi
          local gfs = require("gears.filesystem")
          local themes_path = "${config.xdg.configHome}/awesome"
          local theme = {}

          theme.font = "${config.stylix.fonts.sansSerif.name} ${builtins.toString config.stylix.fonts.sizes.terminal}"

          theme.bg_normal = "${config.lib.stylix.colors.withHashtag.base00}66"
          theme.bg_focus = "#${conAccentColor}"
          theme.bg_urgent = "${config.lib.stylix.colors.withHashtag.base08}"
          theme.bg_minimize = "${config.lib.stylix.colors.withHashtag.base0B}"
          theme.bg_systray = theme.bg_normal

          theme.fg_normal = "${config.lib.stylix.colors.withHashtag.base07}"
          theme.fg_focus = theme.fg_normal
          theme.fg_urgent = theme.fg_normal
          theme.fg_minimize = theme.fg_normal

          theme.gap_single_client = true
          theme.useless_gap = dpi(${builtins.toString conGaps})
          theme.border_width = dpi(${builtins.toString conBorderSize})
          theme.border_normal = "#00000000"
          theme.border_marked = theme.bg_focus
          theme.border_focus = theme.bg_focus
          --snap
          theme.snap_bg = theme.bg_focus
          --notifications
          naughty.config = {
            defaults = {
              ontop = true,
              font = theme.font,
              timeout = 10,
              margin = 20,
              border_width = 1.5,
              font = theme.font,
              fg = beautiful.fg_normal,
              bg = beautiful.bg_normal,
              position = "top_middle",
            },
            padding = 60,
            spacing = 4,
          }
          theme.wallpaper = "${config.stylix.image}"
          return theme
        '';
      };

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

      xdg.configFile."sx/sxrc" = {
        executable = true;
        text = ''
          $TERMINAL &
          xrandr -r ${builtins.toString conRefresh-rate}
          exec ${lib.getExe' pkgs.awesome "awesome"}
        '';
      };

      xdg.configFile."flameshot/flameshot.ini" = {
        # executable = true;
        text = ''
          [General]
          allowMultipleGuiInstances=false
          antialiasingPinZoom=false
          autoCloseIdleDaemon=false
          buttons=@Variant(\0\0\0\x7f\0\0\0\vQList<int>\0\0\0\0\b\0\0\0\0\0\0\0\x1\0\0\0\x2\0\0\0\x6\0\0\0\x12\0\0\0\xf\0\0\0\x16\0\0\0\n)
          contrastOpacity=188
          contrastUiColor=${config.lib.stylix.colors.withHashtag.base0A}
          copyAndCloseAfterUpload=true
          copyOnDoubleClick=true
          copyPathAfterSave=true
          disabledTrayIcon=true
          drawColor=${config.lib.stylix.colors.withHashtag.base08}
          drawFontSize=7
          drawThickness=2
          filenamePattern=screen.png
          historyConfirmationToDelete=false
          saveAfterCopy=false
          saveAsFileExtension=png
          savePath=${conHome}/Pictures/screenshots
          savePathFixed=true
          showDesktopNotification=true
          showHelp=false
          showSidePanelButton=false
          showStartupLaunchMessage=false
          startupLaunch=false
          uiColor=${config.lib.stylix.colors.withHashtag.base0B}
          uploadWithoutConfirmation=false
          useJpgForClipboard=false
          userColors=picker, ${config.lib.stylix.colors.withHashtag.base08}, ${config.lib.stylix.colors.withHashtag.base0B}

          [Shortcuts]
          TYPE_ARROW=A
          TYPE_CIRCLE=C
          TYPE_CIRCLECOUNT=
          TYPE_COMMIT_CURRENT_TOOL=Ctrl+Return
          TYPE_COPY=Ctrl+C
          TYPE_DRAWER=D
          TYPE_EXIT=Ctrl+Q
          TYPE_IMAGEUPLOADER=Return
          TYPE_MARKER=M
          TYPE_MOVESELECTION=Ctrl+M
          TYPE_MOVE_DOWN=Down
          TYPE_MOVE_LEFT=Left
          TYPE_MOVE_RIGHT=Right
          TYPE_MOVE_UP=Up
          TYPE_OPEN_APP=Ctrl+O
          TYPE_PENCIL=P
          TYPE_PIN=
          TYPE_PIXELATE=B
          TYPE_RECTANGLE=R
          TYPE_REDO=Ctrl+Shift+Z
          TYPE_RESIZE_DOWN=Shift+Down
          TYPE_RESIZE_LEFT=Shift+Left
          TYPE_RESIZE_RIGHT=Shift+Right
          TYPE_RESIZE_UP=Shift+Up
          TYPE_SAVE=Ctrl+S
          TYPE_SELECTION=S
          TYPE_SELECTIONINDICATOR=
          TYPE_SELECT_ALL=Ctrl+A
          TYPE_TEXT=T
          TYPE_TOGGLE_PANEL=Space
          TYPE_UNDO=Ctrl+Z
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
          normal = ["${config.stylix.fonts.monospace.name}"];
          size = 13;
        };
      };
      # }}}
    };
  };
  # }}}
}
## vim:foldmethod=marker

