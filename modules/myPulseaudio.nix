{
  pkgs,
  lib,
  ...
}: {
  services.pipewire.enable = lib.mkForce false;

  services.pulseaudio.enable = true;
  services.pulseaudio.support32Bit = true;
  services.pulseaudio.package = pkgs.pulseaudioFull;
  services.pulseaudio.extraConfig = "load-module module-combine-sink";
}
