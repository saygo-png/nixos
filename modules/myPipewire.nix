{...}: {
  # Audio
  # RealtimeKit service, which hands out realtime scheduling priority to user processes on demand. For example, the PulseAudio server uses this to acquire realtime priority.
  security.rtkit.enable = true;

  # Pulseaudio.
  hardware.pulseaudio.enable = false;
  # Disable system-wide ALSA setup, since we're using PipeWire's ALSA emulation. Enabling this can
  # let us use media keys in TTY, for example.
  sound.enable = false;

  # Pipewire
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    jack.enable = true;
    pulse.enable = true;
    alsa.support32Bit = true;
    wireplumber.enable = true;
  };
}
