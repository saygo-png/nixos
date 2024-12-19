{
  pkgs,
  lib,
  ...
}: {
  services.pipewire.enable = lib.mkForce false;

  # services.pipewire.enable = lib.mkForce false;

  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
  hardware.pulseaudio.extraConfig = "load-module module-combine-sink";
}
