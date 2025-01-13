{
  lib,
  pkgs,
  extraLib,
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

  home-manager.users.${conUsername} = {
    osConfig,
    config,
    ...
  }: {
    home.packages =
      [
        pkgs.feh
        pkgs.alsa-tools # used by xmobar
      ]
      ++ (let
        xinitrc = lib.strings.concatLines [
          osConfig.const.xinitBase
          "feh --no-fehbg --bg-center ${config.stylix.image}"
          "exec ${config.xdg.cacheHome}/xmonad/xmonad-x86_64-linux"
        ];
      in
        extraLib.wrapWithXinitrc xinitrc "xmonad");

    xdg.configFile."xmonad" = {
      source = lib.my.relativeToRoot "resources/haskell/xmonad";
      recursive = true;
    };

    programs.xmobar = {
      enable = true;
      extraConfig = let
        green = config.lib.stylix.colors.withHashtag.base0B;
        yellow = config.lib.stylix.colors.withHashtag.base0A;
        orange = config.lib.stylix.colors.withHashtag.base09;
        red = config.lib.stylix.colors.withHashtag.base08;
        dark3 = config.lib.stylix.colors.withHashtag.base03;
      in
        /*
        haskell
        */
        ''
          Config
            { overrideRedirect = False,
              allDesktops = True,
              lowerOnStart = True,
              font = "${config.stylix.fonts.serif.name} Regular ${builtins.toString (config.stylix.fonts.sizes.desktop - 1)}",
              bgColor = "${config.lib.stylix.colors.withHashtag.base00}",
              fgColor = "${config.lib.stylix.colors.withHashtag.base07}",
              position = TopW L 100,
              commands =
                [ Run
                    Cpu
                    [ "--template",
                      "C<total>%",
                      "--Low",
                      "10",
                      "--High",
                      "80",
                      "--high",
                      "${red}",
                      "--normal",
                      "${orange}",
                      "--low",
                      "${green}"
                    ]
                    10,
                  Run Memory [
                    "--template",
                    "M<usedratio>%",
                    "--Low",
                    "20",
                    "--High",
                    "90",
                    "--low",
                    "${green}",
                    "--normal",
                    "${orange}",
                    "--high",
                    "${red}"
                    ] 10,
                  Run Swap [
                    "--template",
                    "S<usedratio>%",
                    "--Low",
                    "10",
                    "--High",
                    "50",
                    "--low",
                    "${green}",
                    "--normal",
                    "${orange}",
                    "--high",
                    "${red}"
                    ] 10,
                  Run Date "%a %Y-%m-%d <fc=${green}>%H:%M</fc>" "date" 10,
                  Run XMonadLog,
                  Run Wireless "wlp3s0" [
                    "--template",
                    "W<quality>",
                    "--Low",
                    "10",
                    "--High",
                    "80",
                    "--low",
                    "${red}",
                    "--normal",
                    "${orange}",
                    "--high",
                    "${green}"
                    ] 10,
                  Run
                    Battery
                    [ "--template",
                      "B<acstatus>",
                      "--Low",
                      "10",
                      "--High",
                      "80",
                      "--low",
                      "${red}",
                      "--normal",
                      "${orange}",
                      "--high",
                      "${green}",
                      "--",
                      -- discharging status
                      "-o",
                      "<left>% <timeleft>",
                      "-O",
                      "<fc=${yellow}>...</fc>",
                      "-i",
                      "<fc=${green}>V</fc>"
                    ]
                    50,
                  Run
                    DynNetwork
                    [ "--template",
                      "<tx>kBs <rx>kBs",
                      "--Low",
                      "5000",
                      "--High",
                      "15000",
                      "--low",
                      "${green}",
                      "--normal",
                      "${orange}",
                      "--high",
                      "${red}"
                    ]
                    10
                ],
              sepChar = "%",
              alignSep = "}{",
              template = "%XMonadLog% }{%battery%<fc=${dark3}>|</fc>%wlp3s0wi% %dynnetwork%<fc=${dark3}>|</fc>%cpu%<fc=${dark3}>|</fc>%memory% %swap%<fc=${dark3}>|</fc>%date%"
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
