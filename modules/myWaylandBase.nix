{
  lib,
  pkgs,
  ...
}: {
  imports = lib.my.withModules [
    "myFlameshot.nix"
    "waybar/waybar.nix"
  ];

  # Polkit (needed for window managers)
  security.polkit.enable = lib.mkDefault true;

  # NixOS is retarded and turns on lightdm by default.
  services.xserver.displayManager.lightdm.enable = false;

  nixpkgs.overlays = [
    (_: prev: {
      flameshot = prev.flameshot.override (_: {
        enableWlrSupport = true;
      });
    })
  ];

  environment.systemPackages = with pkgs; [
    swaybg
    flameshot
    wl-clipboard # wl-copy and wl-paste for copy/paste from stdin / stdout
  ];
}
