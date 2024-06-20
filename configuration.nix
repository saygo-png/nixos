{
  pkgs,
  lib,
  inputs,
  ...
}: let
  constants = {
    hostname = "nixos"; # Change manually in flake.nix
    username = "samsepi0l";
    accentColor = "7d8618"; #7d8618 Hacky!!! Add extra color to stylix.
    home = "/home/${constants.username}";
    flake-path = "${constants.home}/nixos";
  };
in {
  imports = [
    ./hardware-configuration.nix
    inputs.stylix.nixosModules.stylix
    inputs.home-manager.nixosModules.default
  ];

  #######################
  # Essential or basic. #
  #######################

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "${constants.hostname}";
  networking.networkmanager.enable = true;

  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "Europe/Warsaw";

  services.xserver.xkb = {
    layout = "pl";
    options = "caps:escape";
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
  programs.hyprland.enable = true;

  # System packages.
  environment.systemPackages = with pkgs; [
    # Nix.
    nh # Nix helper
    nvd
    nil # Nix LSP
    nix-output-monitor
    alejandra # Nix formatter

    # Other.
    wl-clipboard
    cliphist

    # Haskell
    ghc

    libqalculate
    wget
    eza
    trashy
    tmux
    dash
    git
    fzf
    ripgrep
    gnumake
    fd
    xdg-utils
    xdg-desktop-portal-gtk
    xdg-desktop-portal-hyprland
    libnotify

    # GUI.
    rofi-wayland
    mangohud
    pcmanfm-qt

    # Shellscripts.
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
        set -o pipefail -e -u
        shopt -s failglob
        TARGET="duck.com"
        # Infinite loop for continuous pinging
        while true; do
          if ! fping -c1 -t500 -o "$TARGET" | tee -a /dev/tty | grep -E -q 'timed out|Name or service not known'; then
           sleep 0.1
        else
           notify-send "Ping Failed" "Could not ping $TARGET"
          fi
        done
      '')

    (writeShellScriptBin
      "pizzatimer"
      ''
        #!/bin/bash
        ${pkgs.termdown}/bin/termdown 15m && \
        for i in {1..10}; do ${pkgs.libnotify}/bin/notify-send "Pizza is done."; done
      '')
  ];

  ########################
  # Hardware or drivers. #
  ########################

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };
  services.xserver.videoDrivers = ["amdgpu"];

  ##########
  # Games. #
  ##########

  # This is the command for running all 3 programs at once that u put into steam
  # gamemoderun gamescope -w 1920 -h 1080 -f -- mangohud %command%
  programs.steam.extraCompatPackages = [
    pkgs.proton-ge-bin
  ];
  programs.steam.enable = true;
  programs.steam.gamescopeSession.enable = true;
  programs.gamemode.enable = true;

  ##########
  # NixOs. #
  ##########

  nixpkgs.config.allowUnfree = false;
  nix.settings.experimental-features = ["nix-command" "flakes"];
  nix.settings.auto-optimise-store = true;
  boot.loader.grub.configurationLimit = 30;
  boot.loader.systemd-boot.configurationLimit = 30;
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
  # NixOS is retarded and turns on lightdm by default.
  services.xserver.displayManager.lightdm.enable = false;

  # Polkit (needed for window managers)
  security.polkit.enable = true;

  # Enable sound with low latency.
  hardware.pulseaudio.enable = false;
  services.pipewire = {
    enable = true;
    pulse.enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    jack.enable = true;
    extraConfig.pipewire."92-low-latency" = {
      context.properties = {
        default.clock.rate = 48000;
        default.clock.quantum = 16;
        default.clock.min-quantum = 16;
        default.clock.max-quantum = 16;
      };
    };
  };

  # Disable touchpad support (enabled default in most desktops).
  services.libinput.enable = false;
  # Disable mouse acceleration.
  services.libinput.mouse.accelProfile = "flat";

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
  };

  ###########
  # Visuals #
  ###########

  # Fonts.
  fonts = {
    packages = [
      # Main font.
      pkgs.courier-prime
      (pkgs.nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
      pkgs.noto-fonts-cjk-serif
      pkgs.noto-fonts-cjk-sans
      pkgs.noto-fonts-emoji
      pkgs.noto-fonts
    ];

    fontconfig = {
      enable = true;
      antialias = true;
      hinting.enable = true;
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
  stylix.image = ./resources/wallpaper.png;
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
    desktop = 10;
    popups = 10;
  };

  stylix.opacity = {
    applications = 0.6;
    terminal = 0.6;
    desktop = 0.6;
    popups = 0.6;
  };

  ################
  # HOME MANAGER #
  ################

  home-manager = {
    extraSpecialArgs = {inherit inputs;};
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
          "la" = "${lib.getExe pkgs.eza} -a";
          "ll" = "${lib.getExe pkgs.eza} -l";
          "rt" = "${lib.getExe pkgs.trashy}";
          "shutdown" = "poweroff";
          "qcalc" = "${lib.getExe pkgs.libqalculate}";
          "search" = "sudo find / -maxdepth 99999999 2>/dev/null | ${lib.getExe pkgs.fzf} -i -q $1";
        };

        sessionVariables = {
          # Default programs.
          PAGER = lib.getExe pkgs.moar;
          # Systemd is retarded and doesnt use normal pager variable :DDDDD
          SYSTEMD_PAGER = lib.getExe pkgs.moar;
          OPENER = "${pkgs.xdg-utils}/bin/xdg-open";
          VISUAL = "nvim";
          EDITOR = "nvim";
          SUDO_EDITOR = "nvim";
          TERMINAL = lib.getExe pkgs.alacritty;
          TERMINAL_PROG = lib.getExe pkgs.alacritty;
          BROWSER = lib.getExe pkgs.librewolf;
          # Firefox hardware decode.
          MOZ_X11_EGL = 1;
          NO_AT_BRIDGE = 1;
          # Unreal engine .net cli tool turn off telemetry.
          DOTNET_CLI_TELEMETRY_OPTOUT = "true";
        };

        packages = with pkgs; [
          # GUI.
          librewolf # Browser
          keepassxc # Password manager
          rhythmbox # Music player
          inkscape # Vector
          krita # Painting
          foliate # Ebook reader
          anki # Flashcards
          # hydrus # File manager

          # Command line.
          pulsemixer # Volume control
          zoxide # Cd alternative
          mpv # Video player
          gomuks # TUI matrix client
          cbonsai # Ascii tree animation
          hyprpicker # Color picker
          slurp # Screenshot assistant
          swappy # Quick drawing on images
          termdown # Timer
          tldr # Man alternative
          htop # TUI task manager
          moar # Pager
        ];

        # Binary blobs.
        sessionPath = [
          "${constants.home}/bin"
        ];
        file."bin/tmux-mem-cpp".source = ./resources/tmux-mem-cpp;
      };
      # Development, internal.
      programs.command-not-found.enable = false;
      programs.nix-index.enable = true;
      programs.bash.enable = true;
      programs.zoxide.enable = true;
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
          set-option -sa terminal-features ',alacritty:RGB'
          set-option -g default-terminal "tmux-256color"

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
        enableCompletion = true;
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
      programs.vscode = {
        enable = true;
        package = pkgs.vscodium;
        extensions = with pkgs.vscode-extensions; [
          bbenoist.nix
          kamadorueda.alejandra
          jdinhlife.gruvbox
          jnoortheen.nix-ide
          tekumara.typos-vscode
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
        };
      };
      stylix.targets.alacritty.enable = true;
      programs.nixvim = {
        enable = true;
        extraPackages = with pkgs; [
          codespell # Spelling.
          stylua # Lua formatter
          rust-analyzer # Rust LSP
          vim-language-server
          typos-lsp
          vale # Linter
          haskellPackages.fourmolu # Haskell formatter
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
          isort
          shellcheck # Bash linter
          hadolint # Docker linter
          nodePackages.jsonlint
          mypy # Python type checker
          black # Python formatter
          yapf # Python formatter
          ruff # Python linter
        ];
        highlightOverride = {
          # hi noCursor blend=100 cterm=strikethrough
          # hi ModeMsg guifg=#7d8618
          # hi MsgArea guifg=#7d8618
          noCursor.blend = 100;
          statusline.bg = "NONE";
          statusline.fg = "#${constants.accentColor}";
          CursorLineNr.fg = "#${constants.accentColor}";
          CursorLineNr.bg = "#${config.stylix.base16Scheme.base01}"; # Gray indentline
          MsgArea.fg = "#${constants.accentColor}";
          MiniIndentscopeSymbol.fg = "#${config.stylix.base16Scheme.base01}"; # Gray indentline
        };
        opts = {
          # Indents
          expandtab = true;
          tabstop = 2;
          shiftwidth = 2;
          softtabstop = 2;
          autoindent = true;
          breakindent = true; # Indent when wrapping

          # Delay on switching to normal mode.
          ttimeoutlen = 0;

          # Incremental search.
          hlsearch = true;
          incsearch = true;
          updatetime = 100;

          # Relative numberline on the left.
          number = true;
          relativenumber = true;

          # Color current line number
          cursorline = true;
          cursorlineopt = "number";

          # Smartcase search and ripgrep.
          ignorecase = true;
          smartcase = true;
          grepprg = "rg --vimgrep";
          grepformat = "%f:%l:%c:%m";

          # Folds.
          foldmethod = "indent";
          foldenable = false;

          # More space.
          cmdheight = 0;

          # Prevents screen jumping and needed for gitsigns
          signcolumn = "yes";

          # Show some whitespace.
          list = true;
          listchars = "tab:▸ ,trail:·,nbsp:␣";

          # Better completion.
          completeopt = ["menuone" "noselect" "noinsert"];

          # Always keep 8 lines above/below cursor unless at start/end of file
          scrolloff = 8;

          # Use conform-nvim for gq formatting. ('formatexpr' is set to vim.lsp.formatexpr(), so you can format lines via gq if the language server supports it)
          formatexpr = "v:lua.require'conform'.formatexpr()";

          # (https://neovim.io/doc/user/options.html#'laststatus')
          laststatus = 3;
        };
        globals = {
          mapleader = " ";
        };
        extraConfigVim = builtins.readFile ./resources/nvim-extraConfig.vim;
        # extraConfigLuaPost = ''
        #   vim.cmd [[
        #     ]]
        # '';
        package = pkgs.neovim-unwrapped;
        # enableMan = true;
        clipboard.register = "unnamedplus";
        colorscheme = "gruvbox";
        colorschemes.gruvbox = {
          enable = true;
          settings = {
            transparent_mode = true;
            undercurl = false;
            underline = false;
            strikethrough = false;
          };
        };
        extraPlugins = [
          pkgs.vimPlugins.vim-startuptime
          pkgs.vimPlugins.vim-unimpaired
        ];
        plugins = {
          # nix.enable = true;
          surround.enable = true;
          telescope.enable = true;
          flash = {
            enable = true;
            labels = "asdfghjklqwertyuiopzxcvbnm";
            search = {
              mode = "fuzzy";
            };
            jump = {
              autojump = true;
            };
            label = {
              uppercase = false;
              rainbow = {
                enabled = true;
                shade = 5;
              };
            };
          };
          nvim-colorizer.enable = true;
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
              comment = {
                options = {
                  customCommentString = ''
                    <cmd>lua require("ts_context_commentstring.internal").calculate_commentstring() or vim.bo.commentstring<cr>
                  '';
                };
              };
              align = {};
              trailspace = {};
            };
          };

          trouble = {
            enable = true;
            settings = {
              auto_close = true;
            };
          };

          treesitter-textobjects.enable = true;
          treesitter = {
            enable = true;
            ensureInstalled = [
              "all"
            ];
            nixvimInjections = true;
          };

          lsp = {
            enable = true;
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
              rust-analyzer.enable = true;

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
              haskell = ["fourmolu"];
              lua = ["stylua"];
              nix = ["alejandra"];
              # Use the "*" filetype to run formatters on all filetypes.
              "*" = ["codespell" "trim_whitespace"];
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
            };
          };
          cmp-buffer.enable = true;
          cmp-snippy.enable = true;
          luasnip = {
            enable = true;
            extraConfig = {
              enable_autosnippets = true;
            };
            fromVscode = [
              {
                lazyLoad = false;
                paths = "${pkgs.vimPlugins.friendly-snippets}";
              }
            ];
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
          };
          oil = {
            enable = true;
            settings.defaultFileExplorer = true;
          };

          cmp = {
            enable = true;
            autoEnableSources = false;
            settings = {
              autocomplete = true;
              experimental = {ghost_text = true;};
              snippet = {expand = "snippy";};
              sources = [
                {name = "luasnip";}
                {name = "snippy";}
                {name = "path";}
                {name = "buffer";}
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
            mode = ["n"];
            action = "gP";
            key = "P";
          }
          {
            mode = ["n"];
            action = "gp";
            key = "p";
          }
          {
            mode = ["n"];
            action = "\"_C";
            key = "C";
            options.desc = "Change till end of line to void";
          }
          {
            mode = ["n" "v"];
            action = "\"_c";
            key = "c";
            options.desc = "Change to void";
          }
          {
            mode = ["n" "v"];
            action = "\"_d$";
            key = "D";
            options.desc = "Delete until end of line to void";
          }
          {
            mode = ["n"];
            action = "\"_dd";
            key = "dd";
            options.desc = "Delete line to void";
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
          # {
          #   mode = "n";
          #   action = "<lt><lt><esc>";
          #   key = "<lt>";
          #   options.desc = "Indent less";
          #   options.silent = true;
          # }
          # {
          #   mode = "n";
          #   action = ">><esc>";
          #   key = ">";
          #   options.desc = "Indent more";
          #   options.silent = true;
          # }
          {
            mode = ["n" "x" "o"];
            key = "s";
            action = "<cmd>lua require('flash').jump()<cr>";
            options = {
              desc = "Flash";
            };
          }
          {
            mode = ["n" "x" "o"];
            key = "S";
            action = "<cmd>lua require('flash').treesitter()<cr>";
            options = {
              desc = "Flash Treesitter";
            };
          }
          {
            mode = ["x" "o"];
            key = "R";
            action = "<cmd>lua require('flash').treesitter_search()<cr>";
            options = {
              desc = "Treesitter Search";
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
          {
            mode = "n";
            action = "<cmd>HopWord<CR>";
            key = "<Leader><Leader>";
            options.desc = "Hop";
          }
        ];

        autoCmd = [
          {
            event = ["BufWritePre"];
            command = "lua require(\"conform\").format()";
            desc = "Autoformat";
          }
          {
            event = ["BufReadPost"];
            pattern = ["*"];
            command = "normal!'\"";
            desc = "Open at last location";
          }
        ];
      };

      programs.rofi = {
        package = pkgs.rofi-wayland;
        enable = true;
        extraConfig = {
          modi = "window,run,drun";
          font = "Courier Prime 14";
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
        };
        theme = {
          "*" = {
            normal-background = lib.mkForce (config.lib.formats.rasi.mkLiteral "rgba (0, 0, 0, 0%)");
            alternate-normal-background = lib.mkForce (config.lib.formats.rasi.mkLiteral "rgba (0, 0, 0, 0%)");
            gruvbox-dark-bg0 = lib.mkForce (config.lib.formats.rasi.mkLiteral "rgba (21, 22, 3, 60%)");
            gruvbox-dark-bg0-soft = lib.mkForce (config.lib.formats.rasi.mkLiteral "rgba (21, 22, 3, 60%)");
          };
        };
      };

      wayland.windowManager.hyprland = {
        systemd.enable = true;
        xwayland.enable = true;
        enable = true;
        settings = {
          # Autostart.
          debug = {
            disable_logs = true;
          };
          exec-once = [
            "systemctl --user import-environment QT_QPA_PLATFORMTHEME WAYLAND_DISPLAY XDG_CURRENT_DESKTOP"
            "systemctl --user import-environment &"
            "hash dbus-update-activation-environment 2>/dev/null &"
            "dbus-update-activation-environment --systemd &"
            "${lib.getExe pkgs.lxqt.lxqt-policykit} &"

            "wl-clip-persist --clipboard both"
            "${lib.getExe pkgs.swaybg} -m fill -i ${./resources/wallpaper.png} &"
            "hyprctl dispatch exec '[workspace 2 silent] $BROWSER' &"
            "hyprctl dispatch exec '[workspace 1 silent] $TERMINAL' &"
            # "sleep 1 && swaylock"
            # "poweralertd &"
            # "waybar &"
            # "mako &"
            "wl-paste --watch cliphist store &"
          ];

          input = {
            kb_layout = "pl";
            kb_options = "caps:escape";
            repeat_delay = 300;
            repeat_rate = 30;
            accel_profile = "flat";
            numlock_by_default = false;
            follow_mouse = 1;
            sensitivity = -0.9;
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
            disable_autoreload = false;
            disable_splash_rendering = true;
            animate_manual_resizes = true;
            #   always_follow_on_dnd = true;
            #   layers_hog_keyboard_focus = true;
            enable_swallow = true;
            #   focus_on_activate = true;
          };

          dwindle = {
            # no_gaps_when_only = false;
            force_split = 0;
            special_scale_factor = 1.0;
            split_width_multiplier = 1.0;
            use_active_for_splits = true;
            pseudotile = "yes";
            preserve_split = "yes";
          };

          master = {
            # new_is_master = true;
            # special_scale_factor = 1;
            no_gaps_when_only = false;
          };

          decoration = {
            #   rounding = 0;

            blur = {
              enabled = false;
              size = 1;
              passes = 1;
              # size = 4;
              # passes = 2;
              brightness = 0.5;
              contrast = 1.400;
              ignore_opacity = true;
              noise = 2;
              new_optimizations = true;
              xray = true;
            };

            drop_shadow = true;

            shadow_ignore_window = true;
            shadow_offset = "5 5";
            shadow_range = 15;
            shadow_render_power = 2; # 3
            "col.shadow" = lib.mkForce "rgba(0000007F)";
          };

          animations = {
            enabled = true;

            bezier = [
              "fluent_decel, 0, 0.2, 0.4, 1"
              "easeOutCirc, 0, 0.55, 0.45, 1"
              "easeOutCubic, 0.33, 1, 0.68, 1"
              "easeinoutsine, 0.37, 0, 0.63, 1"
            ];

            animation = [
              # Windows
              "windowsIn, 1, 3, easeOutCubic, popin 30%" # window open
              "windowsOut, 1, 3, fluent_decel, popin 70%" # window close.
              "windowsMove, 1, 2, easeinoutsine, slide" # everything in between, moving, dragging, resizing.

              # Fade
              "fadeIn, 1, 3, easeOutCubic" # fade in (open) -> layers and windows
              "fadeOut, 1, 2, easeOutCubic" # fade out (close) -> layers and windows
              "fadeSwitch, 0, 1, easeOutCirc" # fade on changing activewindow and its opacity
              "fadeShadow, 1, 10, easeOutCirc" # fade on changing activewindow for shadows
              "fadeDim, 1, 4, fluent_decel" # the easing of the dimming of inactive windows
              "border, 1, 2.7, easeOutCirc" # for animating the border's color switch speed
              "borderangle, 1, 30, fluent_decel, once" # for animating the border's gradient angle - styles: once (default), loop
              "workspaces, 1, 4, easeOutCubic, fade" # styles: slide, slidevert, fade, slidefade, slidefadevert
            ];
          };
          # bindd = [
          #   "$mainMod, Q, Close active, killactive,"
          #   "$mainMod, SHIFT + Space, Toggle floating, togglefloating,"
          # ];

          # Bind accepts "flags" after "bind".
          # "e" in "binde" means that a key can be held down to repeat an action.
          # You can add multiple flags, without order like so "bindde".
          binde = [
            # TODO: add these binds:
            # "$mainMod SHIFT, Escape, Hard kill, exec, shutdown-script"

            #  add proper alt tab support using "hycov" plugin
            #  add descriptions to each key

            # Show keybinds list.
            "$mainMod, shiftH, exec, show-keybinds"

            # Close program.
            "$mainMod, Q, killactive,"

            # Toggle floating.
            "$mainMod, shiftSpace, togglefloating,"

            "$mainMod, f, Fullscreen, fullscreen, 0"

            # Open terminal.
            "$mainMod, Return, exec, $TERMINAL"

            # Run common programs.
            "$mainMod, b, exec, hyprctl dispatch exec '[workspace 2 silent] $BROWSER'"

            # Program launcher.
            "$mainMod, Space, exec, pkill rofi || rofi -show drun"

            # Color picker
            "$mainMod, c ,exec, hyprpicker -a"

            # Dunno
            "$mainMod, P, pseudo,"
            "$mainMod, S, togglesplit,"

            # Screenshot.
            ", Print, exec, ${lib.getExe pkgs.grim} -c -g \"$(${lib.getExe pkgs.slurp} -w 0)\" -t png - | ${pkgs.wl-clipboard}/bin/wl-copy"
            "$mainMod, e, exec, ${pkgs.wl-clipboard}/bin/wl-paste | ${lib.getExe pkgs.swappy} -f -"

            # Cycle programs.
            "ALT, Tab, workspace, m+1"
            # "ALT, Tab, bringactivetotop,"
            "$mainMod, Tab, exec, pkill rofi || rofi -show window"

            # Switch focus.
            "$mainMod, h, movefocus, l"
            "$mainMod, l, movefocus, r"
            "$mainMod, k, movefocus, u"
            "$mainMod, j, movefocus, d"

            # Switch workspace.
            "$mainMod, 1, workspace, 1"
            "$mainMod, 2, workspace, 2"
            "$mainMod, 3, workspace, 3"
            "$mainMod, 4, workspace, 4"
            "$mainMod, 5, workspace, 5"
            "$mainMod, 6, workspace, 6"
            "$mainMod, 7, workspace, 7"
            "$mainMod, 8, workspace, 8"
            "$mainMod, 9, workspace, 9"
            "$mainMod, 0, workspace, 10"

            # Same as above, but switch to the workspace.
            "$mainMod SHIFT, 1, movetoworkspacesilent, 1"
            "$mainMod SHIFT, 2, movetoworkspacesilent, 2"
            "$mainMod SHIFT, 3, movetoworkspacesilent, 3"
            "$mainMod SHIFT, 4, movetoworkspacesilent, 4"
            "$mainMod SHIFT, 5, movetoworkspacesilent, 5"
            "$mainMod SHIFT, 6, movetoworkspacesilent, 6"
            "$mainMod SHIFT, 7, movetoworkspacesilent, 7"
            "$mainMod SHIFT, 8, movetoworkspacesilent, 8"
            "$mainMod SHIFT, 9, movetoworkspacesilent, 9"
            "$mainMod SHIFT, 0, movetoworkspacesilent, 10"
            "$mainMod CTRL, c, movetoworkspace, empty"

            # Move to workspace.
            "$mainMod SHIFT, h, movewindow, l"
            "$mainMod SHIFT, l, movewindow, r"
            "$mainMod SHIFT, k, movewindow, u"
            "$mainMod SHIFT, j, movewindow, d"

            # Resizing.
            "$mainMod CTRL + s, h, resizeactive, -100 0"
            "$mainMod CTRL + s, l, resizeactive, 100 0"
            "$mainMod CTRL + s, k, resizeactive, 0 -100"
            "$mainMod CTRL + s, j, resizeactive, 0 100"

            # Floating move.
            "$mainMod ALT, H, moveactive,  -100 0"
            "$mainMod ALT, L, moveactive, 100 0"
            "$mainMod ALT, K, moveactive, 0 -100"
            "$mainMod ALT, J, moveactive, 0 100"

            # media and volume controls
            # "$mainMod,Equal,exec, pamixer -i 2"
            # "$mainMod,Minus,exec, pamixer -d 2"
            # ",XF86AudioMute,exec, pamixer -t"
            # ",XF86AudioPlay,exec, playerctl play-pause"
            # ",XF86AudioNext,exec, playerctl next"
            # ",XF86AudioPrev,exec, playerctl previous"
            # ",XF86AudioStop, exec, playerctl stop"
            # "$mainMod, mouse_down, workspace, e-1"
            # "$mainMod, mouse_up, workspace, e+1"

            # clipboard manager
            "$mainMod, V, exec, cliphist list | ${pkgs.rofi-wayland}/bin/rofi -dmenu | cliphist decode | ${pkgs.wl-clipboard}/bin/wl-copy"
          ];

          # mouse binding
          bindm = [
            "$mainMod, mouse:272, movewindow"
            "$mainMod, mouse:273, resizewindow"
          ];

          windowrule = [
            "float,imv"
            "center,imv"
            "size 1200 725,imv"
            "float,mpv"
            "center,mpv"
            "tile,Aseprite"
            "size 1200 725,mpv"
            "float,title:^(float_kitty)$"
            "center,title:^(float_kitty)$"
            "size 950 600,title:^(float_kitty)$"
            "float,audacious"
            "workspace 8 silent, audacious"
            "pin,rofi"
            "float,rofi"
            "noborder,rofi"
            "tile, neovide"
            "idleinhibit focus,mpv"
            "float,udiskie"
            "float,title:^(Transmission)$"
            "float,title:^(Volume Control)$"
            "float,title:^(Librewolf — Sharing Indicator)$"
            "move 0 0,title:^(Librewolf — Sharing Indicator)$"
            "size 700 450,title:^(Volume Control)$"
            "move 40 55%,title:^(Volume Control)$"
          ];

          windowrulev2 = [
            # Hide border on unfocused windows
            # "noborder, focus:0"

            "suppressevent maximize, class:.*"

            "float, title:^(Picture-in-Picture)$"
            "opacity 1.0 override 1.0 override, title:^(Picture-in-Picture)$"
            "pin, title:^(Picture-in-Picture)$"
            "opacity 1.0 override 1.0 override, title:^(.*imv.*)$"
            "opacity 1.0 override 1.0 override, title:^(.*mpv.*)$"
            "opacity 1.0 override 1.0 override, class:(Aseprite)"
            "opacity 1.0 override 1.0 override, class:(Unity)"
            "idleinhibit focus, class:^(mpv)$"
            "idleinhibit fullscreen, class:^(Librewolf)$"
            "float,class:^(pavucontrol)$"
            "float,class:^(SoundWireServer)$"
            "float,class:^(.sameboy-wrapped)$"
            "float,class:^(file_progress)$"
            "float,class:^(confirm)$"
            "float,class:^(dialog)$"
            "float,class:^(download)$"
            "float,class:^(notification)$"
            "float,class:^(error)$"
            "float,class:^(confirmreset)$"
            "float,title:^(Open File)$"
            "float,title:^(branchdialog)$"
            "float,title:^(Confirm to replace files)$"
            "float,title:^(File Operation Progress)$"
          ];
        };

        extraConfig = "
           monitor=, highres@highrr, auto, 1
           xwayland {
             force_zero_scaling = true
           }
        ";
      };
      services.flameshot = {
        enable = true;
      };
    };
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];

  system.stateVersion = "24.05"; # Dont change
}
