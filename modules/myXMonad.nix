{
  lib,
  pkgs,
  config,
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

  home-manager.users.${conUsername} = {osConfig, ...}: {
    home.packages = [
      pkgs.feh
      (let
        xinitrc = lib.strings.concatLines [
          osConfig.const.xinitBase
          "feh --bg-center ${config.stylix.image}"
          "exec xmonad"
        ];
      in
        lib.my.wrapWithXinitrc xinitrc "xmonad")

      pkgs.xmobar
    ];

    xdg.configFile."xmonad" = {
      source = lib.my.relativeToRoot "resources/haskell/xmonad";
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
