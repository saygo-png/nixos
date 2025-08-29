{
  config,
  pkgs,
  conUsername,
  ...
}: let
  inherit (config.lib.stylix.colors) withHashtag;
  style =
    #css
    ''
      * {
        font-family: "monospace";
        font-size: 11pt;

        border: none;
        border-radius: 0;
        min-height: 0px;
        padding: 0px;
        margin: 0px;
      }

      tooltip, tooltip * {
        color: ${withHashtag.base04};
        background:  ${withHashtag.base01};
      }

      window#waybar {
        background: rgba(0, 0, 0, 0);
        color: ${withHashtag.base05};
      }

      #window {
        color: ${withHashtag.base05};
        font-weight: bold;
      }

      #workspaces button {
        background: transparent;
        color: #7d8618;
        font-weight: bold;
      }

      #workspaces button.active {
        color: #1b1d1e;
        background: #7d8618;
      }

      #workspaces button.urgent {
        color: #1b1d1e;
        background: ${withHashtag.base08};
      }

      #mode {
        background: ${withHashtag.base0E};
        color: #1b1d1e;
      }

      #cpu,
      #workspaces,
      #window,
      #tray,
      #clock,
      #memory,
      #battery,
      #network,
      #custom-disk_root,
      #backlight,
      #pulseaudio,
      #mode {
        padding: 0 4px;
        margin: 0 2px;
        margin-bottom: -2px;
        margin-right: -2px;
      }

      #window {
        padding-left: 0px;
      }

      #workspaces {
        padding-left: 0px;
        margin-left: 0px;
      }

      #tray {
        margin-bottom: 1px;
      }

      #battery icon {
        color: red;
      }
      @keyframes blink {
        to {
          background-color: ${withHashtag.base0E};
        }
      }

      #battery.warning:not(.charging) {
        background-color: #7d8618;
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
        background: ${withHashtag.base08};
      }
    '';
in {
  home-manager.users.${conUsername} = {
    stylix.targets.waybar.enable = false;

    xdg.configFile."waybar/style.css" = {
      text = style;
      onChange = ''
        ${pkgs.procps}/bin/pkill -u $USER -USR2 waybar || true
      '';
    };
  };
}
