{
  lib,
  pkgs,
  conUsername,
  conFlakePathRel,
  ...
}: {
  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Polkit (needed for window managers)
  security.polkit.enable = lib.mkDefault true;

  # Also needed for secrets
  services.gnome.gnome-keyring.enable = true;

  # NixOS is retarded and turns on lightdm by default.
  services.displayManager.defaultSession = lib.mkDefault "none+xmonad";
  services.xserver.displayManager = lib.mkDefault {
    startx.enable = true;
    lightdm.enable = false;
  };

  environment.systemPackages = with pkgs; [
    xclip # Xorg wl-clipboard
    flameshot # X11 screenshot tool
  ];

  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    # flake = {
    #   enable = true;
    #   compiler = "ghc947";
    # };
    # config = builtins.readFile ../xmonad/xmonad.hs;
    enableConfiguredRecompile = false;
  };

  home-manager.users.${conUsername} = {
    osConfig,
    config,
    ...
  }: {
    xsession.windowManager.xmonad = {
      enableContribAndExtras = true;
      enable = true;
    };
  };
}
