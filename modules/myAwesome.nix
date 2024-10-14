{
  lib,
  pkgs,
  conGaps,
  conHome,
  conUsername,
  conBorderSize,
  conAccentColor,
  conFlakePathRel,
  conRefresh-rate,
  ...
}: {
  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Polkit (needed for window managers)
  security.polkit.enable = lib.mkDefault true;

  # NixOS is retarded and turns on lightdm by default.
  services.displayManager.defaultSession = lib.mkDefault "none+awesome";
  services.xserver.displayManager = lib.mkDefault {
    startx.enable = true;
    lightdm.enable = false;
  };

  # X11 window manager for games
  services.xserver.windowManager.awesome = {
    enable = true;
    package = pkgs.awesome;
  };

  environment.systemPackages = with pkgs; [
    xclip # Xorg wl-clipboard
    flameshot # X11 screenshot tool
  ];

  home-manager.users.${conUsername} = {config, ...}: {
    home = {
      file = {
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
            exec awesome
            # while true; do
            #   dbus-launch --sh-syntax --exit-with-session awesome
            # done
          '';
        };
      };
    };

    xdg.configFile."sx/sxrc" = {
      executable = true;
      text = config.home.file.".xinitrc".text;
    };

    xdg.configFile."awesome/" = {
      recursive = true;
      source = "${conFlakePathRel}/resources/awesome";
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
  };
}
