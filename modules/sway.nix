{
  lib,
  pkgs,
  conUsername,
  ...
}: {
  imports = lib.my.withModules ["waylandBase.nix"];

  home-manager.users.${conUsername} = {
    osConfig,
    config,
    ...
  }: {
    stylix.targets.sway.enable = false;
    wayland.windowManager.sway = {
      enable = true;
      package = pkgs.swayfx;

      wrapperFeatures.gtk = true;
      systemd.enable = true;
      extraConfig = ''
        titlebar_padding 1
        titlebar_border_thickness 0
      '';
      config = {
        colors = {};
        modifier = "Mod4";
        window.titlebar = false;
        startup = [
          {command = "${lib.getExe pkgs.swaybg} -m fill -i ${config.stylix.image}";}
          {command = "${config.home.sessionVariables.TERMINAL}";}
        ];
        bars = [
          {
            position = "top";
            statusCommand = null;
            fonts = {
              names = ["monospace"];
              size = 12.0;
            };
            colors = {
              background = config.lib.stylix.colors.base01;
              focusedWorkspace = {
                background = config.lib.stylix.colors.base01;
                border = config.lib.stylix.colors.base01;
                text = config.lib.stylix.colors.base05;
              };
              inactiveWorkspace = {
                background = config.lib.stylix.colors.base01;
                border = config.lib.stylix.colors.base01;
                text = config.lib.stylix.colors.base03;
              };
            };
          }
        ];
        focus = {
          followMouse = false;
          newWindow = "urgent";
        };
        keybindings = let
          mod = "Mod4";
          swaylock-cmd = lib.concatStringsSep " " [
            "${pkgs.swaylock-effects}/bin/swaylock"
            "--daemonize"
            "--ignore-empty-password"
            "--clock"
            "--indicator"
            "--line-uses-inside"
            "--indicator-radius 100"
            "--indicator-thickness 7"
            "--fade-in 0.5"
          ];
        in {
          # Workspace things
          "${mod}+1" = "workspace 1";
          "${mod}+2" = "workspace 2";
          "${mod}+3" = "workspace 3";
          "${mod}+4" = "workspace 4";
          "${mod}+5" = "workspace 5";
          "${mod}+6" = "workspace 6";
          "${mod}+7" = "workspace 7";
          "${mod}+8" = "workspace 8";
          "${mod}+9" = "workspace 9";
          "${mod}+0" = ''[app_id="notes"] scratchpad show;sticky enable'';
          "${mod}+Shift+1" = "move container to workspace 1";
          "${mod}+Shift+2" = "move container to workspace 2";
          "${mod}+Shift+3" = "move container to workspace 3";
          "${mod}+Shift+4" = "move container to workspace 4";
          "${mod}+Shift+5" = "move container to workspace 5";
          "${mod}+Shift+6" = "move container to workspace 6";
          "${mod}+Shift+7" = "move container to workspace 7";
          "${mod}+Shift+8" = "move container to workspace 8";
          "${mod}+Shift+9" = "move container to workspace 9";
          "${mod}+Shift+0" = "move container to workspace 0";

          # Basic wm features
          "${mod}+q" = "kill";
          "${mod}+f" = "fullscreen toggle";
          "${mod}+h" = "focus left";
          "${mod}+j" = "focus down";
          "${mod}+k" = "focus up";
          "${mod}+l" = "focus right";
          "${mod}+Shift+h" = "move left";
          "${mod}+Shift+j" = "move down";
          "${mod}+Shift+k" = "move up";
          "${mod}+Shift+l" = "move right";
          "${mod}+v" = "splitv";
          "${mod}+t" = "splith";
          "${mod}+Control+space" = "floating toggle";
          "${mod}+Shift+r" = "exec swaymsg reload && notify-send 'sway config reloaded'";

          # Launchers
          "${mod}+space" = ''exec --no-startup-id rofi -show drun'';
          "${mod}+Shift+space" = ''exec --no-startup-id rofi -show run'';
          "Alt+space" = ''exec --no-startup-id rofi -show window'';
          "${mod}+Return" = "exec --no-startup-id ${config.home.sessionVariables.TERMINAL}";
          "${mod}+d" = "exec --no-startup-id d3-autocast-menu";
          "${mod}+b" = "exec --no-startup-id ${config.home.sessionVariables.BROWSER}";

          "${mod}+a" = "focus parent";
          "${mod}+r" = "mode resize";
          "${mod}+u" = ''[app_id="file-manager"] scratchpad show'';
          "${mod}+Shift+s" = ''exec flameshot gui'';
          "Mod1+Control+l" = "exec ${swaylock-cmd}";
          "Mod1+Control+m" = "exec --no-startup-id volumectl mute";
          "XF86AudioRaiseVolume" = "exec --no-startup-id volumectl raise";
          "XF86AudioLowerVolume" = "exec --no-startup-id volumectl lower";
          "XF86AudioMute" = "exec --no-startup-id volumectl %";
        };
        input = {
          "type:pointer" = {
            accel_profile = osConfig.services.libinput.mouse.accelProfile;
            pointer_accel = lib.strings.floatToString osConfig.const.accelSpeed;
          };
          "type:keyboard" = {
            xkb_layout = "pl";
            xkb_options = osConfig.services.xserver.xkb.options;
            repeat_delay = builtins.toString osConfig.services.xserver.autoRepeatDelay;
            repeat_rate = builtins.toString osConfig.services.xserver.autoRepeatInterval;
          };
        };
        gaps = {
          inner = osConfig.const.gaps;
          smartGaps = true;
          smartBorders = "on";
        };
        output = let
          width = builtins.toString osConfig.const.screenWidth;
          height = builtins.toString osConfig.const.screenHeight;
          refreshRate = builtins.toString osConfig.const.refreshRate;
        in {
          HDMI-A-2 = {
            resolution = "${width}x${height}@${refreshRate}Hz";
            scale = "1";
          };
          eDP-1 = {
            resolution = "${width}x${height}@${refreshRate}Hz";
            scale = "1";
          };
        };
      };
    };
  };
}
