{
  lib,
  pkgs,
  config,
  conHome,
  conUsername,
  ...
}: {
  imports = [
    (lib.my.relativeToRoot "modules/x11/myPicom.nix")
    ({config, ...}: {
      options = {
        const = config.constLib.mkConstsFromSet {
          xinitBase =
            # bash
            ''
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
              picom &
            '';
        };
      };
    })
  ];

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Polkit (needed for window managers)
  security.polkit.enable = lib.mkDefault true;

  # NixOS is retarded and turns on lightdm by default.
  services.xserver.displayManager = lib.mkDefault {
    startx.enable = true;
    sx.enable = true;
    lightdm.enable = false;
  };

  # We make sx use a wrapped xorg, since it does not parse xserverrc like startx, making it
  # not use a lot of nix options like extraLayouts in xkb.
  nixpkgs.overlays = [
    (final: prev: {
      xorgWrapperForSx = prev.writeShellScriptBin "Xorg" ''
        exec ${prev.xorg.xorgserver}/bin/X ${toString config.services.xserver.displayManager.xserverArgs} "$@"
      '';

      sx = prev.sx.overrideAttrs (_oldAttrs: {
        postInstall = ''
          patsh -p "${final.xorgWrapperForSx}/bin:$PATH" -f $out/bin/sx -s ${builtins.storeDir}

          install -Dm755 -t $out/share/xsessions ${
            prev.makeDesktopItem {
              name = "sx";
              desktopName = "sx";
              comment = "Start a xorg server";
              exec = "sx";
            }
          }/share/applications/sx.desktop
        '';
      });
    })
  ];

  environment.systemPackages = with pkgs; [
    xclip # Xorg wl-clipboard
    flameshot # X11 screenshot tool
    picom # Compositor
  ];

  home-manager.users.${conUsername} = {config, ...}: {
    home = {
      file =
        lib.mkIf ("${config.xresources.path}".source or false)
        {
          # auto xrdb
          ${config.xresources.path}.onChange = ''
            [[ -z "$\{DISPLAY:-}" ]] && echo Display not set && exit 0
            run ${lib.getExe pkgs.xorg.xrdb} "${config.xresources.path}"
          '';
        };
    };

    xdg.configFile."flameshot/flameshot.ini" = {
      text =
        # INI
        ''
          [General]
          allowMultipleGuiInstances=false
          antialiasingPinZoom=false
          autoCloseIdleDaemon=false
          contrastOpacity=188
          contrastUiColor=${config.lib.stylix.colors.withHashtag.base0A}
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
          disabledGrimWarning=true
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
