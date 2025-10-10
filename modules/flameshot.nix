{
  pkgs,
  conUsername,
  ...
}: {
  environment.systemPackages = [
    pkgs.flameshot

    (pkgs.writeShellApplication {
      name = "flameshot_wrapper";
      runtimeInputs = with pkgs; [flameshot xdotool];
      text = ''
        # This script is called instead of flameshot in awesome to fix an issue
        # where calling flameshot would drop focus, even after taking a screenshot
        focusedwindow_before=$(xdotool getactivewindow)
        flameshot gui
        [ "$focusedwindow_before" = "$(xdotool getactivewindow)" ] && xdotool windowfocus "$focusedwindow_before"
      '';
    })

    (pkgs.writeShellApplication {
      name = "flameshot_wrapper_ocr";
      runtimeInputs = with pkgs; [flameshot xdotool tesseract];
      text = ''
        # This script does what flameshot_wrapper does, and
        # copies the screenshoted text to your clipboard
        focusedwindow_before=$(xdotool getactivewindow)
        message=$(flameshot gui -r -s | tesseract --psm 12 --oem 1 -l eng+rus+fin+pol stdin stdout)
        [ "$focusedwindow_before" = "$(xdotool getactivewindow)" ] && xdotool windowfocus "$focusedwindow_before"
        notify-send "$message"
        echo "$message" | xclip -r -sel clip
      '';
    })

    (pkgs.writeShellApplication {
      name = "flameshot_wrapper_ocr_trans";
      runtimeInputs = with pkgs; [flameshot xdotool tesseract translate-shell libnotify];
      text = ''
        # This script does what flameshot_wrapper does, and
        # translates the screenshotted text, sending a notification with the translation
        focusedwindow_before=$(xdotool getactivewindow)
        notify-send -t 3000 "$(flameshot gui -r -s | tesseract --psm 12 --oem 1 -l eng+rus+fin stdin stdout | trans -brief)"
        [ "$focusedwindow_before" = "$(xdotool getactivewindow)" ] && xdotool windowfocus "$focusedwindow_before"
      '';
    })
  ];
  home-manager.users.${conUsername} = {config, ...}: {
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
          useGrimAdapter=true
          savePath=${config.home.homeDirectory}/Pictures/screenshots
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
          TYPE_SELECT_ALL=Ctrl+A
          TYPE_TEXT=T
          TYPE_TOGGLE_PANEL=Space
          TYPE_UNDO=Ctrl+Z
        '';
    };
  };
}
