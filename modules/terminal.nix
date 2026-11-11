{
  config,
  lib,
  pkgs,
  conUsername,
  ...
}: {
  options.custom = {
    defaultTerminal = {
      package = lib.mkOption {
        default = pkgs.xterm;
        description = "Set a terminal as default everywhere. Expects package";
        type = lib.types.package;
      };
      desktopFile = lib.mkOption {
        description = "Name of the desktop file of the default terminal";
        type = lib.types.str;
      };
    };
  };

  config = let
    inherit (config.custom.defaultTerminal) desktopFile;
    termPkg = config.custom.defaultTerminal.package;
    termExe = lib.getExe termPkg;
  in {
    home-manager.users.${conUsername} = {
      home.sessionVariables = {
        TERMINAL = termExe;
        TERMINAL_PROG = termExe;
      };

      xdg = {
        terminal-exec = {
          enable = true;
          settings.default = [desktopFile];
        };
      };

      programs.rofi.settings.terminal = termExe;
      xdg.desktopEntries = {
        nvim = {
          name = "Neovim wrapper ";
          comment = "Terminal text editor launched in a terminal emulator";
          exec = "${termExe} -e nvim";
          mimeType = ["text/plain"];
          categories = ["Utility" "TextEditor"];
          terminal = false;
          icon = "terminal";
        };
      };
    };
  };
}
