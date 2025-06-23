{
  lib,
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
          patchFile = builtins.toFile "patch.txt" ''
            46,51c46
            <         <img
            <             draggable="false"
            <             src="../shiggy.gif"
            <             alt="shiggy"
            <             role="presentation"
            <         />
            ---
            >         <span class="loader"></span>
          '';
          spinnerCss =
            # css
            let
              inherit (config.lib.stylix.colors) withHashtag;
            in ''
              .loader {
                width: 48px;
                height: 48px;
                border: 5px solid ${withHashtag.base06};
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
        in ''
          rm static/shiggy.gif
          echo ${lib.strings.escapeShellArg spinnerCss} >> static/views/style.css
          cat static/views/style.css
          patch static/views/splash.html ${patchFile}
        '';
      });
    })
  ];

  home-manager.users.${conUsername} = _: {
    stylix.targets.vesktop.enable = false;
    programs.vesktop = {
      enable = true;
      vencord = let
        gruvboxTheme = "gruvbox";
      in {
        useSystem = true;
        settings = {
          plugins = {
            WhoReacted.enabled = true;
            VoiceMessages.enabled = true;
            NoServerEmojis.enabled = true;
            ForceOwnerCrown.enabled = true;
            NoUnblockToJump.enabled = true;
            NoBlockedMessages.enabled = true;
            NoDevtoolsWarning.enabled = true;
            NoOnboardingDelay.enabled = true;
          };
          enabledThemes = ["${gruvboxTheme}.css"];
        };
        themes.${gruvboxTheme} = builtins.readFile "${inputs.gruvbox-vesktop}/gruvbox-dark.theme.css";
      };
    };
  };
}
