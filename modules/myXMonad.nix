{
  lib,
  pkgs,
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
    osConfig,
    config,
    ...
  }: {
    home.packages = [
      pkgs.feh
      pkgs.alsa-tools # used by xmobar
      (let
        xinitrc = lib.strings.concatLines [
          osConfig.const.xinitBase
          "feh --bg-center ${config.stylix.image}"
          "exec ${config.xdg.cacheHome}/xmonad/xmonad-x86_64-linux"
        ];
      in
        lib.my.wrapWithXinitrc xinitrc "xmonad")
    ];

    xdg.configFile."xmonad" = {
      source = lib.my.relativeToRoot "resources/haskell/xmonad";
      recursive = true;
    };

    programs.xmobar = {
      enable = true;
      extraConfig =
        /*
        haskell
        */
        ''
           Config { overrideRedirect = False
          , font     = "${config.stylix.fonts.serif.name} Regular ${builtins.toString (config.stylix.fonts.sizes.desktop - 1)}"
          , bgColor  = "${config.lib.stylix.colors.withHashtag.base00}"
          , fgColor  = "${config.lib.stylix.colors.withHashtag.base07}"
          , position = TopW L 90
          , commands = [ Run Cpu
                           [ "-L", "3"
                           , "-H", "50"
                           , "--high"  , "red"
                           , "--normal", "green"
                           ] 10
                       , Run Memory ["--template", "Mem: <usedratio>%"] 10
                       , Run Swap [] 10
                       , Run Date "%a %Y-%m-%d <fc=#8be9fd>%H:%M</fc>" "date" 10
                       , Run XMonadLog
                       ]
          , sepChar  = "%"
          , alignSep = "}{"
          , template = "%XMonadLog% }{ %alsa:default:Master% | %cpu% | %memory% * %swap% |%date% "
          }
        '';
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
