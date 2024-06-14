let
  constants = {
    hostname = "nixos"; # Change manually in flake.nix
    username = "samsepi0l";
    home = "/home/samsepi0l";
    flake-path = "/home/samsepi0l/nixos";
    system = "x86_64-linux";
    version = "24.05";
  };
in
  {
    pkgs,
    lib,
    # config,
    inputs,
    ...
  }: {
    imports = [
      ./hardware-configuration.nix
      inputs.stylix.nixosModules.stylix
      inputs.home-manager.nixosModules.default
    ];
    # Use the systemd-boot EFI boot loader.
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    networking.hostName = "${constants.hostname}"; # Define your hostname.
    networking.networkmanager.enable = true;

    # Locale.
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
    users.users.samsepi0l = {
      isNormalUser = true;
      extraGroups = ["wheel" "networkmanager"]; # Enable ‘sudo’ for the user.
      shell = pkgs.fish;
    };

    #########
    # NixOs #
    #########

    nix.settings.experimental-features = ["nix-command" "flakes"];
    nix.settings.auto-optimise-store = true;
    boot.loader.grub.configurationLimit = 30;
    boot.loader.systemd-boot.configurationLimit = 30;
    nix.gc = {
      automatic = true;
      dates = "2day";
      options = "--delete-older-than 15d";
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
      extraConfig.pipewire."92-low-latency" = {
        context.properties = {
          default.clock.rate = 48000;
          default.clock.quantum = 8;
          default.clock.min-quantum = 8;
          default.clock.max-quantum = 8;
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

    environment.shellAliases = {
      "ls" = "${pkgs.eza}/bin/eza";
      "rm" = "${pkgs.trashy}/bin/trash";
    };

    environment.sessionVariables = {
      FLAKE = "${constants.flake-path}"; # For nix helper.
    };

    environment.systemPackages = with pkgs; [
      # Nix.
      nh
      nvd
      nix-output-monitor

      # Other.
      wget
      eza
      trashy
      tmux
      dash
      git
      xdg-utils
      xdg-desktop-portal
      xdg-desktop-portal-gtk
      xdg-desktop-portal-hyprland
      libnotify

      # GUI.
      rofi-wayland

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
        "pizzatimer"
        ''
          #!/bin/bash
          ${pkgs.termdown}/bin/termdown 15m && \
          for i in {1..10}; do ${pkgs.libnotify}/bin/notify-send "Pizza is done."; done
        '')
    ];

    #########
    # OTHER #
    #########

    programs.fish.enable = true;

    stylix.enable = true;
    stylix.autoEnable = true;
    stylix.polarity = "dark";
    stylix.image = ./resources/wallpaper.png;
    stylix.targets.grub.useImage = true;
    stylix.targets.nixvim.transparent_bg.main = true;
    stylix.targets.nixvim.transparent_bg.sign_column = true;
    stylix.cursor.package = pkgs.capitaine-cursors-themed;
    stylix.cursor.name = "Capitaine Cursors (Gruvbox)";
    stylix.base16Scheme = {
      base00 = "282828"; # dark  ----
      base01 = "3c3836"; # dark  ---
      base02 = "504945"; # dark  --
      base03 = "665c54"; # dark  -
      base04 = "bdae93"; # light +
      base05 = "d5c4a1"; # light ++
      base06 = "ebdbb2"; # light +++
      base07 = "fbf1c7"; # light ++++
      base08 = "fb4934"; # red
      base09 = "fe8019"; # orange
      base0A = "fabd2f"; # yellow
      base0B = "b8bb26"; # green
      base0C = "8ec07c"; # aqua/cyan
      base0D = "83a598"; # blue
      base0E = "d3869b"; # purple
      base0F = "d65d0e"; # brown
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
        package = pkgs.noto-fonts-emoji;
        name = "Noto Color Emoji";
      };
    };

    stylix.fonts.sizes = {
      applications = 12;
      terminal = 13;
      desktop = 10;
      popups = 10;
    };

    stylix.opacity = {
      applications = 0.8;
      terminal = 0.8;
      desktop = 0.8;
      popups = 0.8;
    };

    ################
    # HOME MANAGER #
    ################

    home-manager = {
      extraSpecialArgs = {inherit inputs;};
      backupFileExtension = "backup"; # h-m breaks without it.

      users.samsepi0l = {
        imports = [
          inputs.nixvim.homeManagerModules.nixvim
          inputs.nix-index-database.hmModules.nix-index
        ];
        home = {
          username = "samsepi0l";
          homeDirectory = "/home/samsepi0l";
          stateVersion = "24.05"; # Dont change

          packages = with pkgs; [
            # GUI.
            librewolf
            keepassxc
            rhythmbox
            inkscape
            krita

            # Tooling.
            nil # Nix lsp
            alejandra # Nix formatter

            # Command line.
            gallery-dl
            zoxide
            btop
            iamb
            cbonsai
            neofetch
            termdown
            tldr
            htop
            moar
          ];

          sessionVariables = {
            # Default programs.
            PAGER = "${pkgs.moar}/bin/moar";
            # Systemd is retarded and doesnt use normal pager variable :DDDDD
            SYSTEMD_PAGER = "${pkgs.moar}/bin/moar";
            OPENER = "${pkgs.xdg-utils}/bin/xdg-open";
            VISUAL = "nvim";
            EDITOR = "nvim";
            SUDO_EDITOR = "nvim";
            TERMINAL = "${pkgs.alacritty}/bin/alacritty";
            TERMINAL_PROG = "${pkgs.alacritty}/bin/alacritty";
            BROWSER = "${pkgs.librewolf}/bin/librewolf";
            # Firefox hardware decode.
            MOZ_X11_EGL = 1;
            NO_AT_BRIDGE = 1;
            # Unreal engine .net cli tool turn off telemetry.
            DOTNET_CLI_TELEMETRY_OPTOUT = "true";
          };
        };
        # Style.
        # # Covered by stylix
        # gtk = {
        #   enable = true;
        #   theme = {
        #     package = lib.mkForce pkgs.gruvbox-gtk-theme;
        #     name = lib.mkForce "Gruvbox-Dark-B-MB";
        #   };
        # };
        # Development, internal.
        programs.command-not-found.enable = false;
        programs.nix-index.enable = true;
        programs.fish = {
          enable = true;
          interactiveShellInit = ''
            set fish_greeting # Disable greeting
          '';
        };
        programs.bash.enable = true;
        programs.zoxide.enable = true;
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
            # env.TERM = "xterm-256color";
            window.dynamic_padding = true;
            window.decorations = "full";
            window.dynamic_title = true;
            scrolling.multiplier = 5;
            selection.save_to_clipboard = false;
            cursor.style.shape = "Underline";
            cursor.style.blinking = "on";
            cursor.unfocused_hollow = false;
            cursor.thickness = 0.10;
            window.padding = {
              x = 8;
              y = 8;
            };
          };
        };
        programs.nixvim = {
          enable = true;
          colorschemes.gruvbox.enable = true;
        };
        programs.rofi = {
          package = pkgs.rofi-wayland;
          enable = true;
        };
        wayland.windowManager.hyprland = {
          systemd.enable = true;
          xwayland.enable = true;
          enable = true;
          settings = {
            # autostart
            exec-once = [
              # "systemctl --user import-environment &"
              # "hash dbus-update-activation-environment 2>/dev/null &"
              # "dbus-update-activation-environment --systemd &"
              # "wl-clip-persist --clipboard both"
              # "swaybg -m fill -i $(find ~/Pictures/wallpapers/ -maxdepth 1 -type f) &"
              # "sleep 1 && swaylock"
              # "hyprctl setcursor Nordzy-cursors 22 &"
              # "poweralertd &"
              # "waybar &"
              # "mako &"
              # "wl-paste --watch cliphist store &"
            ];

            input = {
              kb_layout = "pl";
              kb_options = "caps:escape";
              repeat_delay = 300;
              repeat_rate = 50;
              numlock_by_default = false;
              follow_mouse = 1;
              sensitivity = -0.8;
              touchpad = {
                natural_scroll = true;
              };
            };

            general = {
              "$mainMod" = "SUPER";
              layout = "master";
              gaps_in = 20;
              gaps_out = 30;
              border_size = 2;
              border_part_of_window = false;
              no_border_on_floating = false;
            };

            misc = {
              disable_autoreload = true;
              #   disable_hyprland_logo = true;
              #   always_follow_on_dnd = true;
              #   layers_hog_keyboard_focus = true;
              #   animate_manual_resizes = true;
              enable_swallow = true;
              #   focus_on_activate = true;
            };

            # dwindle = {
            #   no_gaps_when_only = true;
            #   force_split = 0;
            #   special_scale_factor = 1.0;
            #   split_width_multiplier = 1.0;
            #   use_active_for_splits = true;
            #   pseudotile = "yes";
            #   preserve_split = "yes";
            # };

            master = {
              # new_is_master = true;
              # special_scale_factor = 1;
              no_gaps_when_only = true;
            };

            # decoration = {
            #   rounding = 0;
            #   active_opacity = 0.90;
            #   inactive_opacity = 0.90;
            #   fullscreen_opacity = 1.0;

            #   blur = {
            #     enabled = true;
            #     size = 1;
            #     passes = 1;
            #     # size = 4;
            #     # passes = 2;
            #     brightness = 1;
            #     contrast = 1.400;
            #     ignore_opacity = true;
            #     noise = 0;
            #     new_optimizations = true;
            #     xray = true;
            #   };

            #   drop_shadow = true;

            #   shadow_ignore_window = true;
            #   shadow_offset = "0 2";
            #   shadow_range = 20;
            #   shadow_render_power = 3;
            #   # "col.shadow" = "rgba(00000055)";
            # };

            animations = {
              enabled = true;

              # bezier = [
              #   "fluent_decel, 0, 0.2, 0.4, 1"
              #   "easeOutCirc, 0, 0.55, 0.45, 1"
              #   "easeOutCubic, 0.33, 1, 0.68, 1"
              #   "easeinoutsine, 0.37, 0, 0.63, 1"
              # ];

              # animation = [
              #   # Windows
              #   "windowsIn, 1, 3, easeOutCubic, popin 30%" # window open
              #   "windowsOut, 1, 3, fluent_decel, popin 70%" # window close.
              #   "windowsMove, 1, 2, easeinoutsine, slide" # everything in between, moving, dragging, resizing.

              #   # Fade
              #   "fadeIn, 1, 3, easeOutCubic" # fade in (open) -> layers and windows
              #   "fadeOut, 1, 2, easeOutCubic" # fade out (close) -> layers and windows
              #   "fadeSwitch, 0, 1, easeOutCirc" # fade on changing activewindow and its opacity
              #   "fadeShadow, 1, 10, easeOutCirc" # fade on changing activewindow for shadows
              #   "fadeDim, 1, 4, fluent_decel" # the easing of the dimming of inactive windows
              #   "border, 1, 2.7, easeOutCirc" # for animating the border's color switch speed
              #   "borderangle, 1, 30, fluent_decel, once" # for animating the border's gradient angle - styles: once (default), loop
              #   "workspaces, 1, 4, easeOutCubic, fade" # styles: slide, slidevert, fade, slidefade, slidefadevert
              # ];
            };

            bind = [
              # show keybinds list
              "$mainMod, S, exec, show-keybinds"

              # keybindings
              "$mainMod, Return, exec, alacritty"
              "$mainMod, Q, killactive,"
              # "ALT, Return, exec, kitty --title float_kitty"
              # "$mainMod SHIFT, Return, exec, kitty --start-as=fullscreen -o 'font_size=16'"
              # "$mainMod, B, exec, hyprctl dispatch exec '[workspace 1 silent] floorp'"
              # "$mainMod, F, fullscreen, 0"
              # "$mainMod SHIFT, F, fullscreen, 1"
              # "$mainMod, Space, togglefloating,"
              # "$mainMod, D, exec, pkill rofi || rofi --show drun"
              # "$mainMod SHIFT, D, exec, hyprctl dispatch exec '[workspace 4 silent] discord'"
              # "$mainMod SHIFT, S, exec, hyprctl dispatch exec '[workspace 5 silent] SoundWireServer'"
              # "$mainMod, Escape, exec, swaylock"
              # "$mainMod SHIFT, Escape, exec, shutdown-script"
              # "$mainMod, P, pseudo,"
              # "$mainMod, J, togglesplit,"
              # "$mainMod, E, exec, nemo"
              # "$mainMod, C ,exec, hyprpicker -a"

              # screenshot
              # "$mainMod, Print, exec, grimblast --notify --cursor save area ~/Pictures/$(date +'%Y-%m-%d-At-%Ih%Mm%Ss').png"
              # ",Print, exec, grimblast --notify --cursor  copy area"

              # switch focus
              "$mainMod, H, movefocus, l"
              "$mainMod, L, movefocus, r"
              "$mainMod, K, movefocus, u"
              "$mainMod, J, movefocus, d"

              # switch workspace
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

              # same as above, but switch to the workspace
              # "$mainMod SHIFT, 1, movetoworkspacesilent, 1" # movetoworkspacesilent
              # "$mainMod SHIFT, 2, movetoworkspacesilent, 2"
              # "$mainMod SHIFT, 3, movetoworkspacesilent, 3"
              # "$mainMod SHIFT, 4, movetoworkspacesilent, 4"
              # "$mainMod SHIFT, 5, movetoworkspacesilent, 5"
              # "$mainMod SHIFT, 6, movetoworkspacesilent, 6"
              # "$mainMod SHIFT, 7, movetoworkspacesilent, 7"
              # "$mainMod SHIFT, 8, movetoworkspacesilent, 8"
              # "$mainMod SHIFT, 9, movetoworkspacesilent, 9"
              # "$mainMod SHIFT, 0, movetoworkspacesilent, 10"
              # "$mainMod CTRL, c, movetoworkspace, empty"

              # window control
              "$mainMod SHIFT, H, movewindow, l"
              "$mainMod SHIFT, L, movewindow, r"
              "$mainMod SHIFT, K, movewindow, u"
              "$mainMod SHIFT, J, movewindow, d"
              "$mainMod CTRL, H, resizeactive, -80 0"
              "$mainMod CTRL, L, resizeactive, 80 0"
              "$mainMod CTRL, K, resizeactive, 0 -80"
              "$mainMod CTRL, J, resizeactive, 0 80"
              "$mainMod ALT, H, moveactive,  -80 0"
              "$mainMod ALT, L, moveactive, 80 0"
              "$mainMod ALT, K, moveactive, 0 -80"
              "$mainMod ALT, J, moveactive, 0 80"

              # media and volume controls
              # ",XF86AudioRaiseVolume,exec, pamixer -i 2"
              # ",XF86AudioLowerVolume,exec, pamixer -d 2"
              # ",XF86AudioMute,exec, pamixer -t"
              # ",XF86AudioPlay,exec, playerctl play-pause"
              # ",XF86AudioNext,exec, playerctl next"
              # ",XF86AudioPrev,exec, playerctl previous"
              # ",XF86AudioStop, exec, playerctl stop"
              # "$mainMod, mouse_down, workspace, e-1"
              # "$mainMod, mouse_up, workspace, e+1"

              # clipboard manager
              # "$mainMod, V, exec, cliphist list | rofi --dmenu | cliphist decode | wl-copy"
            ];

            # mouse binding
            # bindm = [
            #   "$mainMod, mouse:272, movewindow"
            #   "$mainMod, mouse:273, resizewindow"
            # ];

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
              "float,title:^(Firefox — Sharing Indicator)$"
              "move 0 0,title:^(Firefox — Sharing Indicator)$"
              "size 700 450,title:^(Volume Control)$"
              "move 40 55%,title:^(Volume Control)$"
            ];

            windowrulev2 = [
              "suppressevent maximize, class:.*"
              "float, title:^(Picture-in-Picture)$"
              "opacity 1.0 override 1.0 override, title:^(Picture-in-Picture)$"
              "pin, title:^(Picture-in-Picture)$"
              "opacity 1.0 override 1.0 override, title:^(.*imv.*)$"
              "opacity 1.0 override 1.0 override, title:^(.*mpv.*)$"
              "opacity 1.0 override 1.0 override, class:(Aseprite)"
              "opacity 1.0 override 1.0 override, class:(Unity)"
              "idleinhibit focus, class:^(mpv)$"
              "idleinhibit fullscreen, class:^(firefox)$"
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

          #       extraConfig = "
          #   monitor=,preferred,auto,auto
          #   xwayland {
          #     force_zero_scaling = true
          #   }
          # ";
        };
        services.flameshot = {
          enable = true;
        };

        dconf.settings = {
          "org/gnome/desktop/input-sources" = {
            show-all-sources = true;
            sources = [(lib.gvariant.mkTuple ["xkb" "pl"])];
            xkb-options = ["caps:escape"];
          };
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
