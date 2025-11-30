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
      iJustWantThisNumberToBeMyQuantumIDoNotCareAboutAnyMixMaxValuesOrLimits = 2048;
    in {
      pipewire."92-latency" = {
        "context.properties" = {
          "default.clock.quantum" = iJustWantThisNumberToBeMyQuantumIDoNotCareAboutAnyMixMaxValuesOrLimits;
          "default.clock.min-quantum" = iJustWantThisNumberToBeMyQuantumIDoNotCareAboutAnyMixMaxValuesOrLimits;
          "default.clock.max-quantum" = iJustWantThisNumberToBeMyQuantumIDoNotCareAboutAnyMixMaxValuesOrLimits;
          "default.clock.quantum-limit" = iJustWantThisNumberToBeMyQuantumIDoNotCareAboutAnyMixMaxValuesOrLimits;
        };
      };

      pipewire-pulse."92-latency".context = {
        modules = [
          {
            name = "libpipewire-module-protocol-pulse";
            args = {
              pulse.min.req = "${toString iJustWantThisNumberToBeMyQuantumIDoNotCareAboutAnyMixMaxValuesOrLimits}/48000";
              pulse.default.req = "${toString iJustWantThisNumberToBeMyQuantumIDoNotCareAboutAnyMixMaxValuesOrLimits}/48000";
              pulse.max.req = "${toString iJustWantThisNumberToBeMyQuantumIDoNotCareAboutAnyMixMaxValuesOrLimits}/48000";
              pulse.min.quantum = "${toString iJustWantThisNumberToBeMyQuantumIDoNotCareAboutAnyMixMaxValuesOrLimits}/48000";
              pulse.max.quantum = "${toString iJustWantThisNumberToBeMyQuantumIDoNotCareAboutAnyMixMaxValuesOrLimits}/48000";
            };
          }
        ];
        stream.properties = {
          node.latency = "${toString iJustWantThisNumberToBeMyQuantumIDoNotCareAboutAnyMixMaxValuesOrLimits}/48000";
        };
      };
    };
  };
}
