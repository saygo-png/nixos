{
  lib,
  pkgs,
  inputs,
  config,
  conUsername,
  ...
}: let
  homeConfig = config.home-manager.users.${conUsername};
  configPath = lib.removePrefix (homeConfig.home.homeDirectory + "/") homeConfig.xdg.configHome;
  profileName = "librewolf-" + conUsername;
in {
  nixpkgs.overlays = [
    (_final: prev: {
      librewolf = prev.librewolf.overrideAttrs (oldAttrs: {
        # launch librewolf with user profile
        buildCommand =
          oldAttrs.buildCommand
          + ''
            wrapProgram "$out/bin/librewolf" \
              --set 'HOME' '${homeConfig.xdg.configHome}' \
              --append-flags "${
              lib.concatStringsSep " " [
                "--name librewolf"
                # load librewolf profile with same name as user
                "--profile ${homeConfig.home.homeDirectory}/${configPath}/${profileName}"
              ]
            }"
          '';
      });
    })
  ];

  home-manager.users.${conUsername} = {config, ...}: {
    # Removes .keep file from creating ~/.librewolf and ~/.mozzilla
    home.file.".librewolf/native-messaging-hosts".enable = lib.mkForce false;
    home.file.".mozilla/native-messaging-hosts".enable = lib.mkForce false;

    stylix.targets.librewolf.enable = false;
    programs.librewolf = {
      package = pkgs.librewolf.override (old: {
        extraPolicies =
          (old.extraPolicies or {})
          // {
            NoDefaultBookmarks = false;
            UserMessaging = {
              SkipOnboarding = true;
            };
          };
      });
      inherit configPath;
      enable = true;
      profiles.${profileName} = {
        isDefault = true;
        userChrome = builtins.readFile "${inputs.firefox-onebar}/onebar.css";
        containersForce = true;
        settings = {
          "webgl.disabled" = false;
          "extensions.autoDisableScopes" = 0;
          "media.ffmpeg.vaapi.enabled" = true;
          # "network.cookie.lifetimePolicy" = 0;
          # "privacy.resistFingerprinting" = false;
          "privacy.clearOnShutdown_v2.cache" = false;
          "privacy.clearOnShutdown_v2.cookiesAndStorage" = false;
          "privacy.clearOnShutdown_v2.history" = false;

          # Style
          "browser.toolbars.bookmarks.visibility" = "never";
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        };
        bookmarks = {
          force = true;
          settings = let
            mkBookmarkFolder = name: bookmarks: {inherit name bookmarks;};
            mkBookmark = name: url: {inherit name url;};
          in [
            (mkBookmarkFolder "nix"
              [
                (mkBookmark "nixos options" "https://search.nixos.org/options?")
                (mkBookmark "nix packages" "https://search.nixos.org/packages")
                (mkBookmark "hm options" "https://home-manager-options.extranix.com/")
                (mkBookmark "stylix docs" "https://stylix.danth.me")
                (mkBookmark "nixvim docs" "https://nix-community.github.io/nixvim")
              ])
          ];
        };
        extensions = {
          force = true;
          packages = let
            addons = inputs.firefox-addons.packages.${pkgs.system};
          in [
            addons.ublock-origin
            addons.single-file
            addons.sponsorblock
            addons.return-youtube-dislikes
            addons.dark-background-light-text
          ];
          settings = {
            # Dark background light text
            "jid1-QoFqdK4qzUfGWQ@jetpack" = {
              force = true;
              settings = let
                inherit (config.lib.stylix.colors) withHashtag;
              in {
                default_foreground_color = withHashtag.base06;
                default_background_color = withHashtag.base00;
                default_link_color = withHashtag.base0D;
                default_visited_color = withHashtag.base0E;
                default_active_color = withHashtag.base08;
                default_selection_color = withHashtag.base0A;
              };
            };
          };
        };
        search = {
          # {{{
          enable = true;
          force = true;
          default = "ddg";
          engines = {
            bing.metadata.hidden = true;
            google.metadata.hidden = true;
            ddg = {
              definedAliases = [];
              urls = [
                {
                  template = "https://duckduckgo.com";
                  params = [
                    {
                      name = "q";
                      value = "{searchTerms}";
                    }
                  ];
                }
              ];
            };
            "NixOS Packages" = {
              definedAliases = ["!np"];
              urls = [
                {
                  template = "https://search.nixos.org/packages";
                  params = [
                    {
                      name = "query";
                      value = "{searchTerms}";
                    }
                    {
                      name = "channel";
                      value = "unstable";
                    }
                    {
                      name = "type";
                      value = "packages";
                    }
                  ];
                }
              ];
            };
            "NixOS Options" = {
              definedAliases = ["!no"];
              urls = [
                {
                  template = "https://search.nixos.org/options";
                  params = [
                    {
                      name = "query";
                      value = "{searchTerms}";
                    }
                    {
                      name = "channel";
                      value = "unstable";
                    }
                    {
                      name = "type";
                      value = "packages";
                    }
                  ];
                }
              ];
            };
            "Noogle.dev" = {
              definedAliases = ["!ng" "!noogle"];
              urls = [
                {
                  template = "https://noogle.dev/q";
                  params = [
                    {
                      name = "term";
                      value = "{searchTerms}";
                    }
                  ];
                }
              ];
            };
            "Home Manager Options" = {
              definedAliases = ["!hm"];
              urls = [
                {
                  template = "https://home-manager-options.extranix.com";
                  params = [
                    {
                      name = "query";
                      value = "{searchTerms}";
                    }
                  ];
                }
              ];
            };
          };
        }; # }}}
      };
    };
  };
}
