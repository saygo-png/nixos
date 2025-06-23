# Largely based on https://bennett.dev/auto-link-pipewire-ports-wireplumber/ by Bennett Hardwick
{
  pkgs,
  lib,
  ...
  # fix for https://github.com/NixOS/nixpkgs/issues/389149
}: {
  nixpkgs.overlays = [
    (_final: prev: {
      carla = prev.carla.overrideAttrs (_oldAttrs: {
        pythonPath = [pkgs.python3Packages.rdflib];

        postFixup = ''
          # Also sets program_PYTHONPATH and program_PATH variables
          wrapPythonPrograms
          wrapPythonProgramsIn "$out/share/carla/resources" "$out $pythonPath"

          find "$out/share/carla" -maxdepth 1 -type f -not -name "*.py" -print0 | while read -d "" f; do
            patchPythonScript "$f"
          done

          # patchPythonScript "$out/share/carla/carla_settings.py"

          for program in $out/bin/*; do
            wrapQtApp "$program" \
              --prefix PATH : "$program_PATH:${pkgs.which}/bin" \
              --set PYTHONNOUSERSITE true
          done

          find "$out/share/carla/resources" -maxdepth 1 -type f -not -name "*.py" -print0 | while read -d "" f; do
            wrapQtApp "$f" \
              --prefix PATH : "$program_PATH:${pkgs.which}/bin" \
              --set PYTHONNOUSERSITE true
          done
        '';
      });
    })
  ];

  environment.systemPackages = [
    pkgs.carla
  ];

  environment.etc."/pipewire/pipewire.conf.d/10-default-null-sink.conf".text =
    /*
    JSON-SPA
    */
    ''
      context.objects = [
        {
          factory = adapter
          args = {
            factory.name = support.null-audio-sink
            node.name = "virtual_mic"
            media.class = Audio/Source/Virtual
            node.description = "Virtual microphone device"
            audio.position = [ FL FR ]
            audio.channels = "2"
            monitor.channel-volumes = true
            monitor.passthrough = true
          }
        }
      ]
    '';

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

  systemd.user.services."enable-carla-postprocessing" = {
    description = "Load Carla Rack JACK host";
    wantedBy = ["default.target"];
    environment = {PIPEWIRE_LINK_PASSIVE = "true";};
    serviceConfig = {
      type = "exec";
      ExecStart = lib.mkForce "${lib.getExe' pkgs.carla "carla-rack"} --no-gui %h/Documents/carla.carxp";
    };
  };
}
