{
  pkgs,
  lib,
  conUsername,
  ...
}: {
  home-manager.users.${conUsername} = {
    programs.alacritty = {
      enable = true;
      settings = {
        scrolling.multiplier = 5;
        window = {
          dynamic_title = true;
          dynamic_padding = true;
          padding = lib.genAttrs ["x" "y"] (lib.const 4);
        };
        cursor = {
          style.blinking = "on";
          unfocused_hollow = false;
          style.shape = "Underline";
        };
        selection.save_to_clipboard = false;
        scrolling.history = 5000;
        keyboard.bindings = let
          mkBind = key: mods: action: {inherit key mods action;};
          mkBindChar = key: mods: chars: {inherit key mods chars;};
          mkBindCmd = key: mods: command: {inherit key mods command;};
          resetAndClearScript =
            pkgs.writeShellScriptBin "resetAndClear"
            ''
              #!${lib.getExe pkgs.dash}
              if [ -z "$ALACRITTY_LOG" ]; then exit 1; fi

              TERM_PID="''${ALACRITTY_LOG//[^0-9]/}"
              tty=$(ps o tty= --ppid $TERM_PID)

              echo -e "\ec" > /dev/$tty
            '';
        in [
          (mkBind "Escape" "Alt" "ToggleViMode")

          (mkBind "V" "Control" "Paste")
          (mkBind "C" "Control" "Copy")

          # Interrupt (ctrl + c)
          (mkBindChar "C" "Control|Shift" "\\u0003")

          (mkBindCmd "Delete" "Control" (lib.getExe resetAndClearScript))
        ];
      };
    };
  };
}
