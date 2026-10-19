{
  lib,
  pkgs,
  inputs,
  ...
}: {
  imports = lib.my.withModules ["flameshot.nix"];

  # Saves permission info from access portals.
  custom.persist.home.directories = [".local/share/flatpak/db"];

  environment.systemPackages = with pkgs; [
    inputs.saywallpaper.packages.${pkgs.stdenv.hostPlatform.system}.saywallpaper
    wl-clipboard # wl-copy and wl-paste for copy/paste from stdin / stdout
  ];
}
