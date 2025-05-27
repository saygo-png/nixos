{
  lib,
  pkgs,
  config,
  extraLib,
  conUsername,
  ...
}: let
  hyprfullscreenfix = pkgs.callPackage (lib.my.relativeToRoot "resources/haskell/programs/hyprfullscreenfix") {};
  hyprctl-switch-rofi = extraLib.writeAnyShellApplication {
    name = "hyprctl-switch-rofi";
    shellPackage = pkgs.dash;
    runtimeInputs = [pkgs.coreutils pkgs.rofi-wayland];
    text = builtins.readFile (lib.my.relativeToRoot "resources/scripts/hyprctl-switch-rofi.sh");
  };
in {
  programs.hyprland.enable = true;

  environment.systemPackages = with pkgs; [
    hyprctl-switch-rofi

    hyprland-protocols
    wl-clipboard
    hyprpicker # Color picker
    xdg-desktop-portal-hyprland
  ];

  nixpkgs.overlays = [
    (_: prev: {
      flameshot = prev.flameshot.override (_: {
        enableWlrSupport = true;
      });
    })
  ];

  home-manager.users.${conUsername} = {osConfig, ...}: {
    home.shellAliases = {"hyprland" = "Hyprland";};
    services.hyprpaper.enable = lib.mkForce false; # Enabled by default with hyprland.

    stylix.targets.waybar.enable = false;

    xdg.configFile."waybar/config.jsonc".source = lib.my.relativeToRoot "resources/waybar/config.jsonc";

    programs.waybar = {
      enable = true;
      style = let
        inherit (config.lib.stylix.colors) withHashtag;
        #css
      in ''
        * {
          border: none;
          border-radius: 0;
          font-family: "monospace";
          font-size: 11pt;
          min-height: 0;
          margin: 0px;
        }

        window#waybar {
          background: rgba(0, 0, 0, 0);
          color: gray;
          color: ${withHashtag.base07};
        }

        #window {
          color: #e4e4e4;
          font-weight: bold;
        }

        #workspaces {
          padding: 0px;
          margin: 0px;
        }

        #workspaces button {
          padding: 0 2px;
          margin: 0px;
          background: transparent;
          color: #ff8700;
          /* border: 1px solid #1b1d1e; */
          font-weight: bold;
        }
        #workspaces button:hover {
          box-shadow: inherit;
          text-shadow: inherit;
        }

        #workspaces button.focused {
          background: #e88939;
          background: #00afd7;
          color: #1b1d1e;
        }

        #workspaces button.urgent {
          background: #af005f;
          color: #1b1d1e;
        }

        #mode {
          background: #af005f;
          color: #1b1d1e;
        }
        #clock,
        #battery,
        #cpu,
        #memory,
        #network,
        #pulseaudio,
        #custom-spotify,
        #tray,
        #mode {
          padding: 0 3px;
          margin: 0 2px;
        }

        #battery icon {
          color: red;
        }
        @keyframes blink {
          to {
            background-color: #af005f;
          }
        }

        #battery.warning:not(.charging) {
          background-color: #ff8700;
          color: #1b1d1e;
        }
        #battery.critical:not(.charging) {
          color: white;
          animation-name: blink;
          animation-duration: 0.5s;
          animation-timing-function: linear;
          animation-iteration-count: infinite;
          animation-direction: alternate;
        }

        #network.disconnected {
          background: #f53c3c;
        }

        #custom-spotify {
          color: rgb(102, 220, 105);
        }
      '';

      # https://github.com/johnk1917/Nixrland/blob/78b452855f956249e4d639d1df2a6f2af586a234/hm-modules/waybar/waybar.nix

      # settings = {
      # mainBar = {
      #   margin = "0 3 1 3";
      #   layer = "top";
      #
      #   modules-left = ["custom/distro" "hyprland/workspaces" "hyprland/window"];
      #   modules-center = [];
      #   modules-right = ["battery" "cpu" "memory" "pulseaudio" "network" "tray" "clock"];
      #
      #   /*
      #   Modules configuration
      #   */
      #   "hyprland/workspaces" = {
      #     active-only = "false";
      #     on-scroll-up = "hyprctl dispatch workspace e+1";
      #     on-scroll-down = "hyprctl dispatch workspace e-1";
      #     disable-scroll = "false";
      #     all-outputs = "true";
      #     format = "{icon}";
      #     on-click = "activate";
      #     format-icons = {
      #       "1" = "t";
      #       "2" = "b";
      #       "3" = "";
      #       "4" = "";
      #       "5" = "";
      #       "6" = "";
      #       "7" = "g ";
      #       "8" = "gl ";
      #       "9" = "";
      #       "10" = "p";
      #     };
      #   };
      #
      #   "idle_inhibitor" = {
      #     format = "{icon}";
      #     format-icons = {
      #       activated = " ";
      #       deactivated = " ";
      #     };
      #   };
      #
      #   "tray" = {
      #     spacing = 8;
      #   };
      #
      #   "clock" = {
      #     tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
      #     format = " {:%H:%M}";
      #     format-alt = " {:%A, %B %d, %Y}";
      #   };
      #
      #   "cpu" = {
      #     format = "c {usage}%";
      #     tooltip = "false";
      #   };
      #
      #   "memory" = {
      #     format = "m {}%";
      #   };
      #
      #   "backlight" = {
      #     format = "{icon}{percent}%";
      #     format-icons = ["󰃞 " "󰃟 " "󰃠 "];
      #     on-scroll-up = "light -A 1";
      #     on-scroll-down = "light -U 1";
      #   };
      #
      #   "battery" = {
      #     states = {
      #       warning = "30";
      #       critical = "15";
      #     };
      #     format = "{icon}{capacity}%";
      #     tooltip-format = "{timeTo} {capacity}%";
      #     format-charging = "󱐋{capacity}%";
      #     format-plugged = " {capacity}%";
      #     format-alt = "{time} {icon}";
      #     format-icons = ["  " "  " "  " "  " "  "];
      #   };
      #
      #   "network" = {
      #     format-wifi = "wifi{essid} {signalStrength}%";
      #     format-ethernet = "{ifname}";
      #     format-linked = "{ifname}(No IP)";
      #     format-disconnected = "Disconnected";
      #     on-click = "wifi-menu";
      #     on-click-release = "sleep 0";
      #     tooltip-format = "{essid} {signalStrength}%";
      #   };
      #
      #   "pulseaudio" = {
      #     format = "v {volume}% {format_source}";
      #     format-muted = "m {format_source}";
      #
      #     tooltip-format = "{desc} {volume}%";
      #     on-click = "pactl set-sink-mute @DEFAULT_SINK@ toggle";
      #     on-click-right = "pactl set-source-mute @DEFAULT_SOURCE@ toggle";
      #     on-click-middle = "pavucontrol";
      #     on-click-release = "sleep 0";
      #     on-click-middle-release = "sleep 0";
      #   };
      #
      #   "custom/distro" = {
      #     format = " ";
      #     on-click = "rofi -show drun";
      #     on-click-release = "sleep 0";
      #   };
      # };
      # };
    };

    wayland.windowManager.hyprland = let
      gaps_in = osConfig.const.gaps;
      gaps_out = osConfig.const.gaps * 2;
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
        debug.disable_logs = false;
        xwayland.force_zero_scaling = true;
        input.sensitivity = lib.strings.floatToString osConfig.const.accelSpeed;
        monitor = [
          ", highres@highrr, auto, 1"
        ];
        # Autostart.
        exec-once = [
          "waybar"
          "${lib.getExe' pkgs.kdePackages.polkit-kde-agent-1 "polkit-kde-authentication-agent-1"} &"
          "${lib.getExe pkgs.swaybg} -m fill -i ${config.stylix.image} &"
          "systemctl --user import-environment PATH &"
          "hash dbus-update-activation-environment 2>/dev/null &"
          "dbus-update-activation-environment --systemd --all &"
          "udiskie &"
          "hyprctl dispatch exec '[workspace 2 silent] $BROWSER' &"
          "hyprctl dispatch exec '[workspace 1 silent] $TERMINAL' &"
        ];

        input = {
          follow_mouse = 2;
          numlock_by_default = false;
          kb_layout = osConfig.services.xserver.xkb.layout;
          kb_options = osConfig.services.xserver.xkb.options;
          repeat_delay = osConfig.services.xserver.autoRepeatDelay;
          repeat_rate = osConfig.services.xserver.autoRepeatInterval;
          accel_profile = osConfig.services.libinput.mouse.accelProfile;
        };

        cursor = {
          no_warps = true;
          hide_on_key_press = false;
        };

        general = {
          "$mainMod" = "SUPER";
          layout = "dwindle";
          inherit gaps_in;
          inherit gaps_out;
          border_size = osConfig.const.borderSize;
          # border_part_of_window = false;
          no_border_on_floating = false;
          "col.active_border" = lib.mkForce "rgba(${osConfig.const.accentColor}FF)";
          "col.inactive_border" = lib.mkForce "rgba(${config.stylix.base16Scheme.base00}00)";
        };

        group = {
          "col.border_active" = lib.mkForce "rgba(${osConfig.const.accentColor}FF)";
          "col.border_inactive" = lib.mkForce "rgba(${osConfig.const.accentColor}00)";
          "col.border_locked_active" = lib.mkForce "rgba(${config.stylix.base16Scheme.base0C}FF)";
          "col.border_locked_inactive" = lib.mkForce "rgba(${config.stylix.base16Scheme.base0C}00)";
          groupbar = {
            gradients = false;
            "col.active" = lib.mkForce "rgba(${osConfig.const.accentColor}FF)";
            "col.inactive" = lib.mkForce "rgba(${osConfig.const.accentColor}00)";
            "col.locked_active" = lib.mkForce "rgba(${config.stylix.base16Scheme.base0C}FF)";
            "col.locked_inactive" = lib.mkForce "rgba(${config.stylix.base16Scheme.base0C}00)";
          };
        };

        misc = {
          enable_swallow = false;
          disable_autoreload = true;
          # Hides text on bottom of the screen.
          disable_hyprland_logo = true;
          animate_manual_resizes = true;
          disable_splash_rendering = true;
          swallow_regex = "^(Alacritty)$";
        };

        dwindle = {
          force_split = 2;
          pseudotile = "yes";
          preserve_split = "yes";
          special_scale_factor = 1.0;
          split_width_multiplier = 1.0;
          use_active_for_splits = true;
        };

        master = {
          new_on_top = false;
          new_status = "slave";
          new_on_active = "after";
        };

        decoration = {
          rounding = 0;
          dim_around = 0.8;
          blur = {
            enabled = false;
            size = 8;
            passes = 2;
            special = true;
            new_optimizations = true;
          };
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

          "$mainMod, y, Toggle float, setfloating,"
          "$mainMod, y, Toggle [p]in, pin,"
          "$mainMod, y, Toggle [p]in, tagwindow, 69PINNED69"

          "$mainMod, v, Toggle float, togglefloating,"
          "$mainMod, v, Toggle [p]in, tagwindow, -69PINNED69"

          "$mainMod, f, [f]ullscreen, exec, ${lib.getExe hyprfullscreenfix}"
          "$mainMod SHIFT, f, [f]ake fullscreen, fullscreenstate, -1, 2"

          "$mainMod, p, Switch keyboard layout, exec, ${lib.getExe hyprctl-switch-rofi}"

          "$mainMod, a, g[a]ps on, exec, hyprctl keyword general:gaps_in ${builtins.toString gaps_in}"
          "$mainMod, a, g[a]ps on, exec, hyprctl keyword general:gaps_out ${builtins.toString gaps_out}"
          "$mainMod SHIFT, a, g[a]ps off, exec, hyprctl keyword general:gaps_in 0"
          "$mainMod SHIFT, a, g[a]ps off, exec, hyprctl keyword general:gaps_out 0"

          "$mainMod, z, Cycle next in active workspace, cyclenext,"
          "$mainMod, x, Center active, centerwindow,"

          "$mainMod, Return, Open terminal, exec, ${lib.getExe config.custom.defaultTerminal}"

          "$mainMod, b, Open [b]rowser, exec, hyprctl dispatch exec '[workspace 2 silent] $BROWSER'"

          "$mainMod, Space, Program launcher, exec, pkill ${lib.getExe pkgs.rofi-wayland} || ${lib.getExe pkgs.rofi-wayland} -show drun"
          "$mainMod SHIFT, Space, Program launcher, exec, pkill ${lib.getExe pkgs.rofi-wayland} || ${lib.getExe pkgs.rofi-wayland} -show run"

          "$mainMod, c, [c]olor picker, exec, ${lib.getExe pkgs.hyprpicker} -a"

          "$mainMod, s, Toggle [s]plit, togglesplit,"

          "$mainMod, d, Set [d]windle layout, exec, hyprctl keyword general:layout \"dwindle\""
          "$mainMod, m, Set [m]aster layout, exec, hyprctl keyword general:layout \"master\""

          ", Print, Screenshot, exec, flameshot gui"
          "$mainMod, e, [e]dit image, exec, ${pkgs.wl-clipboard}/bin/wl-paste | ${lib.getExe pkgs.satty} --filename -"

          "$mainMod, r, [r]ecord, exec, hyprcorder.sh"
          "$mainMod SHIFT, r, [r]ecord area, exec, hyprcorder.sh -a"

          "$mainMod, Tab, Cycle programs, exec, hyprland-next-visible-client.sh next"
          "$ALT, Tab, Open program menu, exec, ${lib.getExe pkgs.rofi-wayland} -show window"

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
        ];

        windowrulev2 = [
          # Needed for gloss window to tile and not focus.
          # "tile, class:^()$"
          # "noinitialfocus, class:^()$"

          "pin, class:^(rofi)"
          "float, class:^(rofi)"
          "tile, class:^(neovide)"

          "pin, class:^(ripdrag)"
          "float, class:^(ripdrag)"
          "idleinhibit focus, class:^(mpv)"

          # Flameshot fixes
          "stayfocused, class:flameshot, title:flameshot"
          "norounding, class:flameshot, title:flameshot"
          "noborder, class:flameshot, title:flameshot"
          "noanim, class:^(flameshot)$"
          "float, class:^(flameshot)$"
          "move 0 0, class:^(flameshot)$"
          "pin, class:^(flameshot)$"

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
          "idleinhibit fullscreen, class:^(firefox)$"

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
  };
}
