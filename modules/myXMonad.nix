{
  lib,
  conUsername,
  conFlakePathRel,
  ...
}: {
  imports = [
    "${conFlakePathRel}/modules/x11/myXorgBase.nix"
  ];

  services.displayManager.defaultSession = lib.mkDefault "none+xmonad";

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
    # osConfig,
    # config,
    ...
  }: {
    xsession.windowManager.xmonad = {
      enableContribAndExtras = true;
      enable = true;
    };
  };
}
