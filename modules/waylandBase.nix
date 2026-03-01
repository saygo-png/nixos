{
  lib,
  pkgs,
  inputs,
  ...
}: {
  imports = lib.my.withModules [
    "flameshot.nix"
    "waybar/waybar.nix"
  ];

  # Polkit (needed for window managers)
  security.polkit.enable = lib.mkDefault true;

  # NixOS is retarded and turns on lightdm by default.
  services.xserver.displayManager.lightdm.enable = false;

  environment.systemPackages = with pkgs; [
    inputs.saywallpaper.packages.${pkgs.stdenv.hostPlatform.system}.saywallpaper
    flameshot
    wl-clipboard # wl-copy and wl-paste for copy/paste from stdin / stdout
  ];
}
