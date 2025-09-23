{
  conUsername,
  lib,
  ...
}: {
  home-manager.users.${conUsername} = {osConfig, ...}: {
    xdg = {
      configFile = {
        "picom.conf".text =
          /*
          nix
          */
          ''
            shadow = false
            shadow-radius = 7
            shadow-offset-x = -7
            shadow-offset-y = -7

            fading = false
            fade-in-step = 0.03
            fade-out-step = 0.03

            frame-opacity = 0.7

            backend = "glx"
            dithered-present = false
            vsync = ${lib.boolToString osConfig.const.vsync}
            detect-rounded-corners = false
            detect-client-opacity = false
            use-ewmh-active-win = true
            unredir-if-possible = true
            unredir-if-possible-delay = 1000
            detect-transient = false
            use-damage = true;
            transparent-clipping = true
            rules: ({
              match =
                      "window_type = 'tooltip' ||"
                      "window_type = 'dropdown_menu'  ||"
                      "window_type = 'popup_menu'  ||"
                      "window_type = 'menu' ||"

                      "window_type = 'utility'  ||"
                      "window_type = 'combo'"
              transparent-clipping = false;
            },
            {
              match = "window_type = 'dock' || "
                      "window_type = 'desktop'";
              corner-radius = 0;
            },
            {
              match = "name = 'Notification'   || "
                      "class_g = 'Conky'       || "
                      "class_g ?= 'Notify-osd' || "
                      "class_g = 'Cairo-clock' || "
                      "_GTK_FRAME_EXTENTS@";
              shadow = false;
            })
          '';
      };
    };
  };
}
