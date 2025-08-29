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
        type = lib.types.string;
      };
    };
  };

  config = let
    inherit (config.custom.defaultTerminal) desktopFile;
    termPkg = config.custom.defaultTerminal.package;
    termExe = lib.getExe termPkg;
  in {
    home-manager.users.${conUsername} = {config, ...}: {
      home.sessionVariables = {
        TERMINAL = termExe;
        TERMINAL_PROG = termExe;
      };

      xdg = {
        terminal-exec = {
          enable = true;
          settings = {
            default = [desktopFile];
          };
        };

        configFile."Thunar/uca.xml".text =
          # XML
          ''
            <?xml version="1.0" encoding="UTF-8"?>
            <actions>
            <action>
              <icon>utilities-terminal</icon>
              <name>Open Terminal Here</name>
              <submenu></submenu>
              <unique-id>1734179588135391-1</unique-id>
              <command>cd %f &amp;&amp; "${termExe}"</command>
              <description>Example for a custom action</description>
              <range></range>
              <patterns>*</patterns>
              <startup-notify/>
              <directories/>
            </action>
            </actions>
          '';

        configFile."kdeglobals" = {
          force = true;
          text = ''
            [General]
            TerminalApplication=${termExe}
          '';
        };
      };

      programs.rofi.terminal = lib.mkIf config.programs.rofi.enable termExe;
      wayland.windowManager.sway.config.terminal = lib.mkIf config.windowManager.sway.enable termExe;
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
