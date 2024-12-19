{lib, ...}: {
  # Disable pulseaudio.
  hardware.pulseaudio.enable = lib.mkForce false;

  # RealtimeKit service, which hands out realtime scheduling priority to user processes on demand. For example, the PulseAudio server uses this to acquire realtime priority.
  security.rtkit.enable = true;

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
