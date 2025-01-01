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
            #################################
            #             Shadows           #
            #################################

            # Enabled client-side shadows on windows. Note desktop windows
            # (windows with '_NET_WM_WINDOW_TYPE_DESKTOP') never get shadow,
            # unless explicitly requested using the wintypes option.
            #
            # Can be set per-window using rules.
            #
            # Default: false
            shadow = false;

            # The blur radius for shadows, in pixels.
            #
            # Default: 12
            shadow-radius = 7;

            # The opacity of shadows.
            #
            # Range: 0.0 - 1.0
            # Default: 0.75
            # shadow-opacity = .75

            # The left offset for shadows, in pixels.
            #
            # Default: -15
            shadow-offset-x = -7;

            # The top offset for shadows, in pixels.
            #
            # Default: -15
            shadow-offset-y = -7;

            # Hex string color value of shadow. Formatted like "#RRGGBB", e.g. "#C0FFEE".
            #
            # Default: #000000
            # shadow-color = "#000000"

            # Crop shadow of a window fully on a particular monitor to that monitor. This is
            # currently implemented using the X RandR extension.
            #
            # Default: false
            # crop-shadow-to-monitor = false

            #################################
            #           Fading              #
            #################################

            # Fade windows in/out when opening/closing and when opacity changes,
            # unless no-fading-openclose is used. Can be set per-window using rules.
            #
            # Default: false
            fading = false;

            # Opacity change between steps while fading in. (0.01 - 1.0, defaults to 0.028)
            fade-in-step = 0.03;

            # Opacity change between steps while fading out. (0.01 - 1.0, defaults to 0.03)
            fade-out-step = 0.03;

            # The time between steps in fade step, in milliseconds. (> 0, defaults to 10)
            # fade-delta = 10

            # Do not fade on window open/close.
            # no-fading-openclose = false

            # Do not fade destroyed ARGB windows with WM frame. Workaround of bugs in Openbox, Fluxbox, etc.
            # no-fading-destroyed-argb = false

            #################################
            #   Transparency / Opacity      #
            #################################

            # Opacity of window titlebars and borders.
            #
            # Range: 0.1 - 1.0
            # Default: 1.0 (disabled)
            frame-opacity = 0.7;

            # Use fixed inactive dim value, instead of adjusting according to window opacity.
            #
            # Default: false
            # inactive-dim-fixed = true

            #################################
            #       General Settings        #
            #################################

            # Enable remote control via D-Bus. See the man page for more details.
            #
            # Default: false
            # dbus = true

            # Daemonize process. Fork to background after initialization. Causes issues with certain (badly-written) drivers.
            # daemon = false

            # Specify the backend to use: `xrender`, `glx`, or `egl`.
            #
            # Default: "xrender"
            backend = "glx"

            # Use higher precision during rendering, and apply dither when presenting the
            # rendered screen. Reduces banding artifacts, but may cause performance
            # degradation. Only works with OpenGL.
            dithered-present = false;

            # Enable/disable VSync.
            #
            # Default: false
            vsync = ${lib.boolToString osConfig.const.vsync}

            # Try to detect windows with rounded corners and don't consider them
            # shaped windows. The accuracy is not very high, unfortunately.
            #
            # Has nothing to do with `corner-radius`.
            #
            # Default: false
            detect-rounded-corners = false;

            # Detect '_NET_WM_WINDOW_OPACITY' on client windows, useful for window managers
            # not passing '_NET_WM_WINDOW_OPACITY' of client windows to frame windows.
            #
            # Default: false
            detect-client-opacity = false;

            # Use EWMH '_NET_ACTIVE_WINDOW' to determine currently focused window,
            # rather than listening to 'FocusIn'/'FocusOut' event. May be more accurate,
            # provided that the WM supports it.
            #
            # Default: false
            use-ewmh-active-win = true

            # Unredirect all windows if a full-screen opaque window is detected,
            # to maximize performance for full-screen windows. Known to cause flickering
            # when redirecting/unredirecting windows.
            #
            # Default: false
            unredir-if-possible = true

            # Delay before unredirecting the window, in milliseconds.
            #
            # Default: 0.
            unredir-if-possible-delay = 1000

            # Use 'WM_TRANSIENT_FOR' to group windows, and consider windows
            # in the same group focused at the same time.
            #
            # Default: false
            detect-transient = false;

            # Use of damage information for rendering. This cause the only the part of the
            # screen that has actually changed to be redrawn, instead of the whole screen
            # every time. Should improve performance.
            #
            # Default: false
            use-damage = true;

            # Make transparent windows clip other windows like non-transparent windows do,
            # instead of blending on top of them. e.g. placing a transparent window on top
            # of another window will cut a "hole" in that window, and show the desktop background
            # underneath.
            #
            # Default: false
            transparent-clipping = true

            # Set the log level. Possible values are:
            #  "trace", "debug", "info", "warn", "error"
            # in increasing level of importance. Case insensitive.
            # If using the "TRACE" log level, it's better to log into a file
            # using *--log-file*, since it can generate a huge stream of logs.
            #
            # Default: "warn"
            # log-level = "warn";

            # Set the log file.
            # If *--log-file* is never specified, logs will be written to stderr.
            # Otherwise, logs will to written to the given file, though some of the early
            # logs might still be written to the stderr.
            # When setting this option from the config file, it is recommended to use an absolute path.
            #
            # log-file = "/path/to/your/log/file"

            # Write process ID to a file.
            # write-pid-path = "/path/to/your/log/file"

            # Rule-based per-window options.
            #
            # See WINDOW RULES section in the man page for how these work.
            rules: ({
              match =
                      "window_type = 'tooltip' ||"
                      # "window_type = 'dialog'  ||"
                      "window_type = 'dropdown_menu'  ||"
                      "window_type = 'popup_menu'  ||"
                      "window_type = 'menu' ||"

                      # "window_type = 'toolbar' ||"
                      # "window_type = 'dock' ||"
                      "window_type = 'utility'  ||"
                      # "window_type = 'splash'  ||"
                      # "window_type = 'normal'  ||"
                      "window_type = 'combo'"
                      # "window_type = 'dnd'"

                      # "window_type = 'notification'";
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
