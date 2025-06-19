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
          # Auto enable extensions
          "extensions.autoDisableScopes" = 0;

          "webgl.disabled" = false;
          "media.ffmpeg.vaapi.enabled" = true;

          "privacy.clearOnShutdown_v2.cache" = false;
          "privacy.clearOnShutdown_v2.cookiesAndStorage" = false;
          "privacy.clearOnShutdown_v2.history" = false;

          "browser.warnOnQuit" = false;
          "browser.warnOnQuitShortcut" = false;

          "middlemouse.paste" = false;
          "browser.tabs.insertAfterCurrent" = true;

          # Prefer dark theme
          "layout.css.prefers-color-scheme.content-override" = 0;

          # Restore previous session
          "browser.startup.page" = 3;

          # Style
          "browser.toolbars.bookmarks.visibility" = "never";
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        };
        bookmarks = {
          force = true;
          settings = let
            mkBookmarkFolder = name: bookmarks: {inherit name bookmarks;};
            mkBookmark = name: url: {
              inherit name;
              url = "https://" + url;
            };
          in [
            (mkBookmarkFolder "nix"
              [
                (mkBookmark "my issues" "github.com/saygo-png/nixos/issues")
                (mkBookmark "nixos options" "search.nixos.org/options?")
                (mkBookmark "nix packages" "search.nixos.org/packages")
                (mkBookmark "nix pr tracker" "nixpk.gs/pr-tracker.html")
                (mkBookmark "hm options" "home-manager-options.extranix.com")
                (mkBookmark "nixpkgs docs" "nixos.org/manual/nixpkgs/unstable")
                (mkBookmark "stylix docs" "stylix.danth.me")
                (mkBookmark "nixvim docs" "nix-community.github.io/nixvim")
              ])

            (mkBookmarkFolder "eso"
              [
                (mkBookmark "warden build" "eso.justlootit.com/eso-builds/warden/warden-healer-eso")
              ])

            (mkBookmarkFolder "knitting"
              [
                (mkBookmark "hat" "bhookedcrochet.com/2022/03/18/everyday-rib-knit-hat")
              ])
          ];
        };
        extensions = {
          force = true;
          packages = let
            addons = inputs.firefox-addons.packages.${pkgs.system};
          in [
            addons.single-file
            addons.sponsorblock
            addons.ublock-origin
            addons.gruvbox-dark-theme
            addons.return-youtube-dislikes
            addons.dark-background-light-text
          ];
          settings = let
            ublock-origin = "uBlock0@raymondhill.net";
            dark-background-light-text = "jid1-QoFqdK4qzUfGWQ@jetpack";
          in {
            ${ublock-origin} = {
              force = true;
              settings.selectedFilterLists = [
                "FIN-0"
                "POL-0"
                "POL-3"
                "plowe-0"
                "easylist"
                "block-lan"
                "urlhaus-1"
                "dpollock-0"
                "easyprivacy"
                "user-filters"
                "easylist-chat"
                "fanboy-social"
                "adguard-mobile"
                "adguard-social"
                "ublock-badware"
                "ublock-filters"
                "ublock-privacy"
                "ublock-unbreak"
                "adguard-cookies"
                "adguard-generic"
                "adguard-spyware"
                "adguard-widgets"
                "curben-phishing"
                "ublock-annoyances"
                "ublock-quick-fixes"
                "adguard-spyware-url"
                "easylist-annoyances"
                "easylist-newsletters"
                "fanboy-cookiemonster"
                "adguard-popup-overlays"
                "easylist-notifications"
                "LegitimateURLShortener"
                "ublock-cookies-adguard"
                "ublock-cookies-easylist"
                "adguard-other-annoyances"
                "fanboy-thirdparty_social"
                "adguard-mobile-app-banners"
                # Third party:
                "https://raw.githubusercontent.com/gijsdev/ublock-hide-yt-shorts/master/list.txt"
              ];
            };
            ${dark-background-light-text} = {
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
                configured_pages = let
                  # This setting sets how the extension works
                  # per website
                  disabled = 0;
                in {
                  "allegro.pl" = disabled;
                  "github.com" = disabled;
                  "poczta.wp.pl" = disabled;
                  "www.vinted.pl" = disabled;
                  "www.reddit.com" = disabled;
                  "www.whitedisplay.com" = disabled;
                };
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
