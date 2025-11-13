{
  lib,
  pkgs,
  inputs,
  config,
  conUsername,
  ...
}: {
  nixpkgs.overlays = [
    (_final: prev: {
      vesktop = prev.vesktop.overrideAttrs (oldAttrs: {
        preConfigurePhases = (oldAttrs.preConfigurePhases or []) ++ ["myPatchImages"];

        myPatchImages = let
          inherit (config.lib.stylix.colors) withHashtag;
          inherit (lib.strings) escapeShellArg;

          patchFile = builtins.toFile "patch.txt" ''
            56c56
            <         <img draggable="false" src="vesktop://assets/splash" alt="" role="presentation" />
            ---
            >         <span class="loader"></span>
            69c69
            < </script>
            \ No newline at end of file
            ---
            > </script>
          '';

          newIconSvg =
            # svg
            ''
              <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="${withHashtag.base05}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                <path d="M4 4h16c1.1 0 2 .9 2 2v12c0 1.1-.9 2-2 2H4c-1.1 0-2-.9-2-2V6c0-1.1.9-2 2-2z"></path>
                <polyline points="22,6 12,13 2,6"></polyline>
              </svg>
            '';

          spinnerCss =
            # css
            ''
              .loader {
                width: 48px;
                height: 48px;
                border: 5px solid ${withHashtag.base05};
                border-bottom-color: transparent;
                border-radius: 50%;
                display: inline-block;
                box-sizing: border-box;
                animation: rotation 1s linear infinite;
              }

              @keyframes rotation {
                0% {
                    transform: rotate(0deg);
                }
                100% {
                    transform: rotate(360deg);
                }
              }
            '';

          rsvg-convert = lib.getExe' pkgs.librsvg "rsvg-convert";
          oldPngIconSize = toString 1024;
        in ''
          echo ${escapeShellArg newIconSvg} | ${rsvg-convert} - -h ${oldPngIconSize} > static/tray.png
          echo ${escapeShellArg spinnerCss} >> static/views/style.css
          patch static/views/splash.html ${patchFile}
        '';
      });
    })
  ];

  custom.persist.home.cache.directories = [".config/vesktop/sessionData"];

  home-manager.users.${conUsername} = {
    stylix.targets.vesktop.enable = false;
    xdg.configFile."vesktop/state.json" = {
      force = true;
      text = builtins.toJSON {
        firstLaunch = false;
      };
    };
    programs.vesktop = {
      enable = true;
      settings = {
        appBadge = false;
        arRPC = false;
        enableSplashScreen = true;
        customTitleBar = false;
        disableMinSize = true;
        minimizeToTray = false;
        tray = true;
        splashTheming = true;
        staticTitle = false;
        hardwareAcceleration = true;
        videoHardwareAcceleration = true;
        discordBranch = "stable";
      };
      vencord = let
        gruvboxTheme = "gruvbox";
      in {
        useSystem = false;
        themes.${gruvboxTheme} = builtins.readFile "${inputs.gruvbox-vesktop}/gruvbox-dark.theme.css"; # IFD?
        settings = {
          enabledThemes = ["${gruvboxTheme}.css"];
          plugins = {
            WhoReacted.enabled = true;
            NoServerEmojis.enabled = true;
            ForceOwnerCrown.enabled = true;
            NoUnblockToJump.enabled = true;
            NoBlockedMessages.enabled = true;
            NoOnboardingDelay.enabled = true;
          };
        };
      };
    };
  };
}
