{lib, ...}: {
  # Disable pulseaudio.
  services.pulseaudio.enable = lib.mkForce false;

  # RealtimeKit service, which hands out realtime scheduling priority to user processes on demand. For example, the PulseAudio server uses this to acquire realtime priority.
  security.rtkit.enable = true;

  # Pipewire
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    jack.enable = true;
    pulse.enable = true;
    alsa.support32Bit = true;

    # Increase latency a bit to stop crackles
    extraConfig = let
      quantum = rec {
        rate = 48000;
        max = default * 2;
        default = 512;
        min = default / 2;
      };
    in {
      pipewire."92-latency" = {
        "context.properties" = {
          "default.clock.quantum" = quantum.default;
          "default.clock.min-quantum" = quantum.min;
          "default.clock.max-quantum" = quantum.max;
        };
      };
      #
      pipewire-pulse."92-latency".context = {
        modules = [
          {
            name = "libpipewire-module-protocol-pulse";
            args = let
              quantumStr = lib.mapAttrs (_: v: "${toString v}/${toString quantum.rate}") quantum;
            in {
              pulse.default.req = quantumStr.default;
              pulse.min.quantum = quantumStr.min;
              pulse.max.quantum = quantumStr.max;
            };
          }
        ];
      };
    };
  };
}
