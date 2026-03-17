{
  lib,
  config,
  ...
}: let
  inherit (config.lib.stylix.colors) withHashtag;
  color-span = content: color: ''<span color="${color}">${content}</span>'';

  bg-text-color = content: color-span content withHashtag.base03;
  base0B = content: color-span content withHashtag.base0B;
  sharedConfig = {
    # Choose the order of the modules
    mainBar = {
      modules-right = [
        "custom/disk_root"
        "cpu"
        "memory"
        "network"
        "backlight"
        "pulseaudio"
        "clock"
        "battery"
        "tray"
      ];

      "custom/disk_root" = {
        format = bg-text-color "d" + "{}";
        interval = 120;
        exec = "free-space";
      };

      cpu = {
        format = bg-text-color "c" + base0B "{usage}" + "%";
        interval = 3;
        tooltip = true;
      };

      memory = {
        format = bg-text-color "m" + base0B "{used:0.1f}" + "G";
        tooltip = true;
      };

      network = {
        format-wifi = "<span color='#589df6'></span> <span color='gray'>{essid}</span> {frequency} <span color='#589df6'>{signaldBm} dB</span> <span color='#589df6'>⇵</span> {bandwidthUpBits}/{bandwidthDownBits}";
        format-ethernet = "connected";
        format-linked = "connected (No IP)";
        format-disconnected = "disconnected";
        tooltip = true;
        interval = 5;
      };

      backlight = {
        format = "{icon} {percent}%";
        format-icons = ["🔅" "🔆"];
      };

      pulseaudio = {
        format = bg-text-color "{icon}" + base0B "{volume}" + "%";
        format-muted = bg-text-color "M" + "{format_source}";
        format-bluetooth = "{icon}{volume}% {format_source}";
        format-bluetooth-muted = bg-text-color "MB" + "{format_source}";

        format-icons = {
          headphones = "h";
          handsfree = "";
          headset = "hs";
          phone = "p";
          portable = "po";
          car = "c";
          default = ["v" "v" "v"];
        };
        on-click = "pactl set-sink-mute @DEFAULT_SINK@ toggle";
        on-click-right = "pactl set-source-mute @DEFAULT_SOURCE@ toggle";
        on-click-middle = "pavucontrol";
      };

      clock = {
        interval = 1;
        format = "{0:%a %Y-%m(%B)-%d}" + " " + base0B "{0:%H:%M}";
        tooltip-format = "{0:%Y-%m-%d}" + " " + base0B "{0:%H:%M:%S}";
      };

      battery = {
        states = {
          warning = 20;
          critical = 10;
        };
        format-icons = ["b" "b" "b" "b" "b"];
      };

      "battery#bat2" = {
        bat = "BAT2";
      };

      tray = {
        spacing = 10;
      };
    };
  };
in {
  imports =
    [
      ({config, ...}: {
        options = {
          const = config.constLib.mkConstsFromSet {
            waybarBase = sharedConfig;
          };
        };
      })
    ]
    ++ lib.my.withModules [
      "waybar/style.nix"
    ];
}
