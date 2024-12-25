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
    home-manager.users.${conUsername} = {config, ...}: {
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

      programs.rofi.terminal = lib.mkIf (config.programs.rofi.enable == true) termExe;
      wayland.windowManager.sway.config.terminal = lib.mkIf (config.windowManager.sway.enable == true) termExe;
      xdg.desktopEntries = lib.mkIf (config.home.sessionVariables == "nvim") {
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
