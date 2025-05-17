{
  config,
  lib,
  pkgs,
  conUsername,
  ...
}: {
  options.custom = {
    defaultTerminal = lib.mkOption {
      default = pkgs.xterm;
      description = "Set a terminal as default everywhere. Expects package";
      type = lib.types.package;
    };
  };

  config = let
    termPkg = config.custom.defaultTerminal;
    termExe = lib.getExe termPkg;
  in {
    # xdg.terminal-exec = {
    #   enable = true;
    #   settings = {
    #     default = ["${termExe}.desktop"];
    #   };
    # };
    home-manager.users.${conUsername} = {
      osConfig,
      config,
      ...
    }: {
      home.sessionVariables = {
        TERMINAL = termExe;
        TERMINAL_PROG = termExe;
      };

      home.packages = [
        (pkgs.writeShellScriptBin "xdg-terminal-exec" ''
          exec "${termExe}" -- "$@"
        '')
      ];

      # TODO make this conditional on something (somehow detect if any kde package is installed?)
      xdg.configFile."kdeglobals".text = ''
        [General]
        TerminalApplication=${termExe}
      '';

      xdg.configFile."Thunar/uca.xml".text =
        lib.mkIf osConfig.programs.thunar.enable
        # XML
        ''
          <?xml version="1.0" encoding="UTF-8"?>
          <actions>
          <action>
            <icon>utilities-terminal</icon>
            <name>Open Terminal Here</name>
            <submenu></submenu>
            <unique-id>1734179588135391-1</unique-id>
            <command>cd %f &amp;&amp; "$TERMINAL"</command>
            <description>Example for a custom action</description>
            <range></range>
            <patterns>*</patterns>
            <startup-notify/>
            <directories/>
          </action>
          </actions>
        '';

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
