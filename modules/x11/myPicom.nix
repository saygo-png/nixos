{
  lib,
  pkgs,
  conUsername,
  ...
}: {
  home-manager.users.${conUsername} = {
    osConfig,
    config,
    ...
  }: {
    xdg = {
      configFile = {
        "picom.conf".text =
          /*
          nix
          */
          ''
            # Enabled client-side shadows on windows. Note desktop windows
            # (windows with '_NET_WM_WINDOW_TYPE_DESKTOP') never get shadow,
            # unless explicitly requested using the wintypes option.
            #
            # shadow = false
            shadow = false

            # Fade windows in/out when opening/closing and when opacity changes,
            #  unless no-fading-openclose is used.
            fading = false

            # Opacity of window titlebars and borders. (0.1 - 1.0, disabled by default)
            # frame-opacity = 1.0
            frame-opacity = 1

            #################################
            #     Background-Blurring       #
            #################################

            # Parameters for background blurring, see the *BLUR* section for more information.
            blur-method = "dual_kawase"
            # blur-size = 12 # kernel and box blur only
            #
            blur-deviation = false # only for gaussian blur
            #
            blur-strength = 4

            # Blur background of semi-transparent / ARGB windows.
            # Bad in performance, with driver-dependent behavior.
            # The name of the switch may change without prior notifications.
            #
            blur-background = false

            # Blur background of windows when the window frame is not opaque.
            # Implies:
            #    blur-background
            # Bad in performance, with driver-dependent behavior. The name may change.
            #
            blur-background-frame = false

            # Use fixed blur strength rather than adjusting according to window opacity.
            blur-background-fixed = true

            # Specify the blur convolution kernel, with the following format:
            # example:
            #   blur-kern = "5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1";
            #
            # blur-kern = ""
            blur-kern = "3x3box"

            # Exclude conditions for background blur.
            # blur-background-exclude = []
            blur-background-exclude = [
              "window_type = 'dock'",
              "window_type = 'desktop'",
              "class_g = 'slop'",
              "class_g = 'argb'",
              "class_g = 'Flameshot'",
              "name = 'Smite'",
              "name = 'Flameshot'",
              "_GTK_FRAME_EXTENTS@:c"
            ];

            #################################
            #       General Settings        #
            #################################

            # Enable remote control via D-Bus. See the man page for more details.
            dbus = false

            # Daemonize process. Fork to background after initialization. Causes issues with certain (badly-written) drivers.
            daemon = true

            # backend, glx is fastest
            backend = "glx"

            # Use higher precision during rendering, and apply dither when presenting the
            # rendered screen. Reduces banding artifacts, but might cause performance
            # degradation. Only works with OpenGL.
            dithered-present = false

            # Enable/disable VSync.
            # vsync = false
            vsync = false

            # Try to detect windows with rounded corners and don't consider them
            # shaped windows. The accuracy is not very high, unfortunately.
            #
            # detect-rounded-corners = false
            detect-rounded-corners = false

            # Use EWMH '_NET_ACTIVE_WINDOW' to determine currently focused window,
            # rather than listening to 'FocusIn'/'FocusOut' event. Might have more accuracy,
            # provided that the WM supports it.
            #
            use-ewmh-active-win = true

            # Unredirect all windows if a full-screen opaque window is detected,
            # to maximize performance for full-screen windows. Known to cause flickering
            # when redirecting/unredirecting windows.
            #
            unredir-if-possible = true

            # Delay before unredirecting the window, in milliseconds. Defaults to 0.
            unredir-if-possible-delay = 5000

            # Conditions of windows that shouldn't be considered full-screen for unredirecting screen.
            # unredir-if-possible-exclude = []

            # GLX backend: Avoid using stencil buffer, useful if you don't have a stencil buffer.
            # Might cause incorrect opacity when rendering transparent content (but never
            # practically happened) and may not work with blur-background.
            # My tests show a 15% performance boost. Recommended.
            #
            glx-no-stencil = true

            # GLX backend: Avoid rebinding pixmap on window damage.
            # Probably could improve performance on rapid window content changes,
            # but is known to break things on some drivers (LLVMpipe, xf86-video-intel, etc.).
            # Recommended if it works.
            #
            glx-no-rebind-pixmap = true

            # Disable the use of damage information.
            # This cause the whole screen to be redrawn every time, instead of the part of the screen
            # has actually changed. Potentially degrades the performance, but might fix some artifacts.
            # The opposing option is use-damage
            #
            no-use-damage = false
            use-damage = true

            # Do not use EWMH to detect fullscreen windows.
            # Reverts to checking if a window is fullscreen based only on its size and coordinates.
            #
            no-ewmh-fullscreen = false

            transparent-clipping = true

            wintypes: {
              tooltip = { fade = false; shadow = false; focus = false; };
              normal = { };
              dock = { shadow = false; };
              dnd = { shadow = false; };
              popup_menu = { shadow = false; focus = false; opacity = 1; };
              dropdown_menu = { transparent-clipping = true;};
              above = { shadow = false; };
              splash = { shadow = false; };
              utility = { focus = false; shadow = false;};
              notification = { shadow = false; transparent-clipping = false;};
              desktop = { shadow = false; };
              menu = { focus = false; };
              dialog = { shadow = false; };
            };
          '';
      };
    };
  };
}
