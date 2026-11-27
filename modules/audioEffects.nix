# Largely based on https://bennett.dev/auto-link-pipewire-ports-wireplumber/ by Bennett Hardwick
{
  system,
  inputs,
  pkgs,
  lib,
  config,
  ...
}: let
  # Carla loads libjack at runtime. Like pw-jack, point it at PipeWire's, which
  # services.pipewire.jack.enable used to do for every program in the session.
  carla = pkgs.symlinkJoin {
    inherit (pkgs.carla) name meta;
    paths = [pkgs.carla];
    nativeBuildInputs = [pkgs.makeWrapper];
    postBuild = ''
      for program in $out/bin/*; do
        wrapProgram "$program" --prefix LD_LIBRARY_PATH : ${config.services.pipewire.package.jack}/lib
      done
    '';
  };
in {
  environment.systemPackages = [
    carla
    inputs.zlequalizer.packages.${system}.zlequalizer
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
          # Highest priority determines default source.
          # Real ALSA mics top out at about 2600 (capture +1000, pro-audio +500, USB +100).
          "priority.session" = 4000;
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
    "autoconnect.lua" = builtins.readFile (lib.my.relativeToRoot "resources/wireplumber-autoconnect.lua");
  };

  custom.persist.home.directories = [".config/falkTX"];

  systemd.user.services."carla-postprocessing" = {
    description = "Load Carla Rack JACK host";
    wantedBy = ["default.target"];
    environment = {PIPEWIRE_LINK_PASSIVE = "true";};
    serviceConfig = {
      Type = "exec";
      ExecStart = "${lib.getExe' carla "carla-rack"} --no-gui %h/Documents/carla.carxp";
    };
  };
}
