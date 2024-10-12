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
    "${conFlakePathRel}/modules/myZSH.nix"
    "${conFlakePathRel}/modules/myTmux.nix"
    "${conFlakePathRel}/modules/myAichat.nix"
    "${conFlakePathRel}/modules/myNeovim.nix"
    "${conFlakePathRel}/modules/myAwesome.nix"
    "${conFlakePathRel}/modules/mySecrets.nix"
    "${conFlakePathRel}/modules/myHyprland.nix"
    "${conFlakePathRel}/modules/myPackages.nix"
    "${conFlakePathRel}/modules/myUnthemedQT.nix"
    "${conFlakePathRel}/modules/myPrismlauncher.nix"
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

  # Remove screentearing on x11
  services.xserver.deviceSection = ''Option "TearFree" "true"'';

  services.xserver.xkb = {
    layout = "pl,plfi";
    options = "grp:sclk_toggle";
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

  # Remap CAPS lock to ESC
  services.udev.extraHwdb = ''
    evdev:atkbd:*
      KEYBOARD_KEY_3a=esc
  '';

  console = {
    useXkbConfig = true;
    font = "${pkgs.terminus_font}/share/consolefonts/ter-132n.psf.gz";
  };

  # Define a user account. Don't forget to set a password with ‘passwd $USERNAME’.
  users.users.${conUsername} = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager" "video"]; # Enable ‘sudo’ for the user.
    # Video allows to set brightness.
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
    nil # Nix LSP
    nh # Nix helper
    alejandra # Nix formatter
    nix-tree # Reverse dependency search
    nix-output-monitor # Pretty nix build output

    # Printing.
    cups
    cups-bjnp
    # cups-brother-hl1110
    # cups-brother-hl1210w
    # cups-brother-hl2260d
    # cups-brother-hl3140cw
    # cups-brother-hll2340dw
    # cups-brother-hll2375dw
    # cups-brother-hll3230cdw
    # cups-brother-mfcl2750dw

    # GUI.
    firefox
    nsxiv # Image viewer
    qalculate-gtk # Gui calculator
    kdePackages.dolphin # File manager

    # CLI.
    ncdu
    rclone
    exiftool
    jq # Json parser
    gcc # C compiling
    vim # Text editor
    wget # Downloader
    fzf # Fuzzy finder
    git # Source control
    udiskie # Auto mount
    gnumake # C compiling
    gtrash # Cli trashcan
    file # File identifier
    libqalculate # Calculator
    udftools # Udf filesystem
    patool # Universal archiver
    ripgrep # Multithreaded grep
    wl-clipboard # Wayland xclip
    xdg-utils # Includes xdg-open
    imagemagick_light # Image identifier
    libnotify # Notifications (notify-send)
    ntfs3g # ntfs filesystem interop (windows fs)

    # Portals (filepickers etc.).
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
  nix = {
    channel.enable = false;
    settings.warn-dirty = false;
    settings.auto-optimise-store = true;
    settings.experimental-features = ["nix-command" "flakes"];
    gc = {
      automatic = true;
      dates = "2day";
      options = "--delete-older-than 15d";
    };
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
      nixos = {
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
  stylix.image = ./resources/wallpaper.png;
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
        defaultApplications = let
          EDITOR = config.home.sessionVariables.EDITOR;
          BROWSER = config.home.sessionVariables.BROWSER;
        in {
          "text/plain" = "${EDITOR}.desktop";
          "text/rhtml" = "${EDITOR}.desktop";
          "text/x-tex" = "${EDITOR}.desktop";
          "text/x-java" = "${EDITOR}.desktop";
          "text/x-ruby" = "${EDITOR}.desktop";
          "text/x-cmake" = "${EDITOR}.desktop";
          "inode/x-empty" = "${EDITOR}.desktop";
          "text/markdown" = "${EDITOR}.desktop";
          "text/x-python" = "${EDITOR}.desktop";
          "text/x-readme" = "${EDITOR}.desktop";
          "text/x-markdown" = "${EDITOR}.desktop";
          "application/json" = "${EDITOR}.desktop";
          "application/x-ruby" = "${EDITOR}.desktop";
          "application/x-yaml" = "${EDITOR}.desktop";
          "application/x-docbook+xml" = "${EDITOR}.desktop";
          "application/x-shellscript" = "${EDITOR}.desktop";

          "inode/directory" = "dolphin.desktop";

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
        stateVersion = "24.05"; # Dont change # CHANGE IT ON UPDATE NO BALLS

        pointerCursor.gtk.enable = true;

        shellAliases = {
          "archive" = "patool";
          "f" = "fzfcd";
          "cp" = "cp -v";
          "rm" = "rm -I";
          "pkill" = "pkill -f";
          "countlines" = "tokei";
          "shutdown" = "poweroff";
          "ls" = "${lib.getExe pkgs.eza}";
          "la" = "${lib.getExe pkgs.eza} -a";
          "ll" = "${lib.getExe pkgs.eza} -l";
          "more" = "${lib.getExe pkgs.moar}";
          "rt" = "${lib.getExe pkgs.gtrash} put";
          "cbonsai" = "cbonsai --screensaver";
          "pmem" = "vmrss"; # [p]rocess [mem]ory
          "date" = ''date +"%A, %d %B %Y, %H:%M:%S"'';
          "qcalc" = "${lib.getExe pkgs.libqalculate}";
          "plan" = "nsxiv ${config.home.homeDirectory}/Sync/notes/plan.png";
          "grep" = "${lib.getExe pkgs.gnugrep} --color=auto";
          "backup" = "sudo borgmatic --verbosity 1 --list --stats";
          "nhoffline" = "nh os switch ${conFlakePath} -- --option substitute false";
          "listinstalledpackages" = "nix-store --query --requisites /run/current-system | cut -d- -f2- | sort -u";
          "search" = "sudo echo got sudo && sudo find / -maxdepth 99999999 2>/dev/null | ${lib.getExe pkgs.fzf} -i -q $1";
          "record" = "${lib.getExe' pkgs.alsa-utils "arecord"} -t wav -r 48000 -c 1 -f S16_LE ${config.home.homeDirectory}/screencaptures/recording.wav";
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
          scribus
          mandelbulber
          anki # Flashcards
          librewolf # Browser
          libreoffice # office
          foliate # Ebook reader
          sayonara # Music player
          zathura # Better for pdfs
          fontforge-gtk # Font editor
          keepassxc # Password manager
          qbittorrent # Torrent client
          swappy # Quick drawing on images
          mission-center # GUI task manager
          localsend # Send via local network
          xdragon # drag items from terminal
          jetbrains.pycharm-community-src # python IDE

          # Dependencies for intersubs for mpv
          (pkgs.python3.withPackages (python312Packages: [
            python312Packages.six
            python312Packages.lxml
            python312Packages.httpx
            python312Packages.numpy
            python312Packages.pyqt5
            python312Packages.thttp
            python312Packages.requests
            python312Packages.beautifulsoup4
          ]))
          socat

          # Command line.
          moar # Pager
          termdown # Timer
          tokei # Line counter
          cbonsai # pretty tree
          p7zip # 7zip archiver
          zoxide # Cd alternative
          htop-vim # TUI task manager
          pulsemixer # Volume control
          ffmpeg # Video and magic editor
          cookiecutter # Project templates
          gmic # Image processing language
          bc # Gnu calculator, needed for vmrss

          # Haskell
          ghc # Haskell LSP
          haskell-language-server # Haskell LSP

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

          run mkdir -p "${config.home.homeDirectory}/.local/share/krita"
          run chmod -R $VERBOSE_ARG u+w,g+w "${config.home.homeDirectory}/.local/share/krita"
          run cp -rf $VERBOSE_ARG "${builtins.toPath ./resources/krita/kritarc}" "${config.xdg.configHome}/kritarc"
          run cp -rf $VERBOSE_ARG "${builtins.toPath ./resources/krita/kritadisplayrc}" "${config.xdg.configHome}/kritadisplayrc"
          run cp -rf $VERBOSE_ARG "${builtins.toPath ./resources/krita/krita-toplevel}"/. "${config.home.homeDirectory}/.local/share/krita"
          run chmod -R $VERBOSE_ARG u+w,g+w "${config.home.homeDirectory}/.local/share/krita"

          run mkdir -p "${config.home.homeDirectory}/.local/share/Anki2"
          run chmod -R $VERBOSE_ARG u+w,g+w "${config.home.homeDirectory}/.local/share/Anki2"
          run cp -rf $VERBOSE_ARG "${builtins.toPath ./resources/anki}"/. "${config.home.homeDirectory}/.local/share/Anki2"
          run chmod -R $VERBOSE_ARG u+w,g+w "${config.home.homeDirectory}/.local/share/Anki2"
        '';

        activation.directories = lib.hm.dag.entryAfter ["writeBoundary"] ''
          run mkdir -p "${config.home.homeDirectory}/Pictures/screenshots"
          run mkdir -p "${config.home.homeDirectory}/backups"

          run mkdir -p "${config.home.homeDirectory}/Desktop"
          run rm -f "${config.home.homeDirectory}/Desktop/Desktop" || true

          run mkdir -p "${config.home.homeDirectory}/screencaptures"
          run ln -s "${config.home.homeDirectory}/screencaptures" "${config.home.homeDirectory}/Desktop/screencaptures" || true
          run rm -f "${config.home.homeDirectory}/screencaptures/screencaptures" || true

          run mkdir -p "${config.home.homeDirectory}/Downloads"
          run ln -s "${config.home.homeDirectory}/Downloads" "${config.home.homeDirectory}/Desktop/Downloads" || true
          run rm -f "${config.home.homeDirectory}/Downloads/Downloads" || true

          run mkdir -p "${config.home.homeDirectory}/Videos"
          run ln -s "${config.home.homeDirectory}/Videos" "${config.home.homeDirectory}/Desktop/Videos" || true
          run rm -f "${config.home.homeDirectory}/Videos/Videos" || true

          run mkdir -p "${config.home.homeDirectory}/Sync"
          run ln -s "${config.home.homeDirectory}/Sync" "${config.home.homeDirectory}/Desktop/Sync" || true
          run rm -f "${config.home.homeDirectory}/Sync/Sync" || true

          run mkdir -p "${config.xdg.configHome}"
          run ln -s "${config.xdg.configHome}" "${config.home.homeDirectory}/Desktop/.config" || true
          run rm -f "${config.home.homeDirectory}/Desktop/.config/.config" || true
          run rm -f "${config.home.homeDirectory}/.config/.config" || true

          run mkdir -p "${config.xdg.dataHome}"
          run ln -s "${config.xdg.dataHome}" "${config.home.homeDirectory}/Desktop/.local" || true
          run rm -f "${config.home.homeDirectory}/Desktop/.local/.local" || true
          run rm -f "${config.home.homeDirectory}/.local/.local" || true
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
        settings = {
          gui.border = "single";
          git = {
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
              key = "<c-a>";
              description = "Pick AI commit";
              command = ''
                aichat "Please suggest 10 commit messages, given the following diff:

                \`\`\`diff
                $(git diff --cached)
                \`\`\`

                **Criteria:**

                1. **Format:** Each commit message must follow the conventional commits format, which is \`<type>(<scope>): <description>\`.
                2. **Relevance:** Avoid mentioning a module name unless it's directly relevant to the change.
                3. **Enumeration:** List the commit messages from 1 to 10, but don't add the number itself.
                4. **Clarity and Conciseness:** Each message should clearly and concisely convey the change made.

                **Commit Message Examples:**

                - fix(app): add password regex pattern
                - test(unit): add new test cases
                - style: remove unused imports
                - refactor(pages): extract common code to \`utils/wait.ts\`

                **Recent Commits on Repo for Reference:**

                \`\`\`
                $(git log -n 10 --pretty=format:'%h %s')
                \`\`\`

                **Output Template**

                Follow this output template and ONLY output raw commit messages without spacing, numbers or other decorations.

                fix(app): add password regex pattern
                test(unit): add new test cases
                style: remove unused imports
                refactor(pages): extract common code to \`utils/wait.ts\`


                **Instructions:**

                - Take a moment to understand the changes made in the diff.
                - Think about the impact of these changes on the project (e.g., bug fixes, new features, performance improvements, code refactoring, documentation updates). It's critical to my career you abstract the changes to a higher level and not just describe the code changes.
                - Generate commit messages that accurately describe these changes, ensuring they are helpful to someone reading the project's history.
                - Remember, a well-crafted commit message can significantly aid in the maintenance and understanding of the project over time.
                - If multiple changes are present, make sure you capture them all in each commit message.

                Keep in mind you will suggest 10 commit messages. Only 1 will be used. It's better to push yourself (esp to synthesize to a higher level) and maybe wrong about some of the 10 commits because only one needs to be good. I'm looking for your best commit, not the best average commit. It's better to cover more scenarios than include a lot of overlap.

                Write your 10 commit messages below in the format shown in Output Template section above." \
                  | fzf --height 70% --ansi --preview "echo {}" --preview-window=up:wrap \
                  | xargs -I {} bash -c '
                      COMMIT_MSG_FILE=$(mktemp)
                      echo "{}" > "$COMMIT_MSG_FILE"
                      ''${EDITOR:-vim} "$COMMIT_MSG_FILE"
                      if [ -s "$COMMIT_MSG_FILE" ]; then
                          git commit -F "$COMMIT_MSG_FILE"
                      else
                          echo "Commit message is empty, commit aborted."
                      fi
                      rm -f "$COMMIT_MSG_FILE"'
              '';
              context = "files";
              subprocess = true;
            }
          ];
        };
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
        package = pkgs.gitAndTools.gitFull;
        aliases = {
          aa = "add -A"; # [A]dd [A]ll
          amend = "commit -a --amend";
          undo = "reset HEAD~1 --mixed";
          dlog = "-c diff.external=difft log --ext-diff";
          dshow = "-c diff.external=difft show --ext-diff";
          deleteGitignored = "rm --cached `git ls-files -i -c --exclude-from=.gitignore`";
          prettylog = "log --pretty=\"(%C(Green)%cr%C(reset)) %C(Cyan)%an: %C(reset)%s\" --date=short";
        };
        extraConfig = {
          user = {
            signingKey = "86B6FCCC3563C00B";
            name = "saygo-png";
            email = "saygo.mail@proton.me";
          };
          color.ui = "auto";
          pull.rebase = true;
          commit.gpgsign = true;
          rerere.enabled = true;
          pull.autoSquash = true;
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
              # Interrupt (ctrl + c)
              key = "C";
              mods = "Control|Shift";
              chars = "\\u0003";
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

      xdg.configFile."wallpaper.png".source = config.stylix.image;

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

