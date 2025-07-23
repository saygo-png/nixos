{
  lib,
  pkgs,
  conUsername,
  ...
}: {
  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Polkit (needed for window managers)
  security.polkit.enable = lib.mkDefault true;

  # NixOS is retarded and turns on lightdm by default.
  services.xserver.displayManager = {
    lightdm.enable = false;
  };

  nixpkgs.overlays = [
    (_: prev: {
      flameshot = prev.flameshot.override (_: {
        enableWlrSupport = true;
      });
    })
  ];

  environment.systemPackages = with pkgs; [
    wl-clipboard # wl-copy and wl-paste for copy/paste from stdin / stdout
    flameshot
    swaybg
  ];

  programs.niri.enable = true;

  home-manager.users.${conUsername} = _: {
    services.polkit-gnome.enable = true;
    xdg.configFile."niri/config.kdl".source = lib.my.relativeToRoot "resources/niri.kdl";
  };
}
