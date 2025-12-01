# Largely based on https://bennett.dev/auto-link-pipewire-ports-wireplumber/ by Bennett Hardwick
{
  inputs,
  pkgs,
  lib,
  ...
}: {
  environment.systemPackages = [
    pkgs.carla
    inputs.zlequalizer.packages.${pkgs.stdenv.hostPlatform.system}.zlequalizer
  ];

  services.pipewire.extraConfig.pipewire."10-default-null-sink" = {
    "context.objects" = [
      {
        factory = "adapter";
        args = {
          "factory.name" = "support.null-audio-sink";
          "node.name" = "virtual_mic";
          "media.class" = "Audio/Source/Virtual";
          "node.description" = "Virtual microphone device";
          "audio.position" = ["FL" "FR"];
          "audio.channels" = "2";
          "monitor.channel-volumes" = true;
          "monitor.passthrough" = true;
        };
      }
    ];
  };

  services.pipewire.wireplumber.extraConfig."99-autoconnect" = {
    "wireplumber.components" = [
      {
        name = "autoconnect.lua";
        type = "script/lua";
        provides = "custom.autoconnect";
      }
    ];

    "wireplumber.profiles" = {
      main = {
        "custom.autoconnect" = "required";
      };
    };
  };

  services.pipewire.wireplumber.extraScripts = {
    "autoconnect.lua" = builtins.readFile (lib.my.relativeToRoot "resources/wireplumber/autoconnect.lua");
  };

  custom.persist.home.directories = [".config/falkTX"];

  systemd.user.services."carla-postprocessing" = {
    description = "Load Carla Rack JACK host";
    wantedBy = ["default.target"];
    environment = {PIPEWIRE_LINK_PASSIVE = "true";};
    serviceConfig = {
      type = "exec";
      ExecStart = lib.mkForce "${lib.getExe' pkgs.carla "carla-rack"} --no-gui %h/Documents/carla.carxp";
    };
  };
}
