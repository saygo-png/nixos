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
    extraConfig.pipewire."92-low-latency" = {
      "context.properties" = {
        "default.clock.quantum" = 64; # increased from 32
        "default.clock.min-quantum" = 32;
        "default.clock.max-quantum" = 128; # increased from 32
      };
    };
  };
}
