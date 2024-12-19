{
  pkgs,
  lib,
  conUsername,
  conFlakePathRel,
  ...
}: {
  imports = [
    "${conFlakePathRel}/modules/x11/myXorgBase.nix"
  ];

  # services.xserver.windowManager.xmonad = {
  #   enable = true;
  #   enableContribAndExtras = true;
  #   # flake = {
  #   #   enable = true;
  #   #   compiler = "ghc947";
  #   # };
  #   config = builtins.readFile "${conFlakePathRel}/resources/xmonad/config.hs";
  #   enableConfiguredRecompile = false;
  # };

  services.displayManager.defaultSession = "none+xmonad";

  home-manager.users.${conUsername} = {
    # osConfig,
    # config,
    ...
  }: {
    home.packages = with pkgs; [
      xmobar
    ];

    xdg.configFile."xmonad" = {
      source = "${conFlakePathRel}/resources/xmonad";
      recursive = true;
    };

    xsession.windowManager.xmonad = {
      enableContribAndExtras = true;
      enable = true;
      extraPackages = hpkgs:
        with hpkgs; [
          xmonad
          xmonad-contrib
          xmonad-extras
        ];
    };
  };
}
