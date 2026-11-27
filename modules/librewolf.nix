{
  lib,
  pkgs,
  inputs,
  system,
  config,
  conUsername,
  ...
}: let
  homeConfig = config.home-manager.users.${conUsername};
  configPath = lib.removePrefix (homeConfig.home.homeDirectory + "/") homeConfig.xdg.configHome;
  profileName = "librewolf-" + conUsername;
in {
  home-manager.users.${conUsername} = {
    # Removes .keep file from creating ~/.librewolf and ~/.mozilla
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
        containersForce = true;
        settings = {
          # Auto enable extensions
          "extensions.autoDisableScopes" = 0;
          "privacy.resistFingerprinting" = false;

          "webgl.disabled" = false;
          "librewolf.webgl.prompt" = false;
          "media.ffmpeg.vaapi.enabled" = true;

          "privacy.clearOnShutdown_v2.cache" = false;
          "privacy.clearOnShutdown_v2.cookiesAndStorage" = false;
          "privacy.clearOnShutdown_v2.history" = false;

          "browser.warnOnQuit" = false;
          "browser.warnOnQuitShortcut" = false;

          "middlemouse.paste" = false;

          # Prefer dark theme
          "layout.css.prefers-color-scheme.content-override" = 0;

          # Restore previous session
          "browser.startup.page" = 3;

          # Style
          "browser.toolbars.bookmarks.visibility" = "never";

          # Performance
          # Save some memory
          "browser.tabs.unloadOnLowMemory" = true;
          "browser.tabs.fadeOutUnloadedTabs" = true;

          # Cache in disk and small ssl tokens.
          # This allows for tracking but i use cookies anyway.
          "network.ssl_tokens_cache_persistence" = true;
          "browser.cache.disk.enable" = true;

          # Disable/lessen some aggressive caching that I don't need
          "dom.ipc.processPrelaunch.enabled" = false;
          "browser.newtab.preload" = false;
          "browser.sessionhistory.max_total_viewers" = 0;
          "browser.sessionstore.max_tabs_undo" = 1;
          "browser.sessionstore.max_serialize_forward" = 1;
          "browser.sessionstore.interval" = 600000;

          # I don't need these on every load
          "reader.parse-on-load.enabled" = false;
          "browser.translations.automaticallyPopup" = false;

          # I'm not disabled i don't need this.
          "accessibility.force_disabled" = 1;
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
            {
              name = "YT shorts player to normal";
              url = ''javascript:(function(){const match=window.location.href.match(/^https?:\/\/(www\.)?youtube\.com\/shorts\/([a-zA-Z0-9_-]+)$/);if(match){window.location.href=%27https://www.youtube.com/watch?v=%27+match[2];}})();'';
            }
            (mkBookmark "my issues" "github.com/saygo-png/nixos/issues")
            (mkBookmark "nixos options" "search.nixos.org/options?")
            (mkBookmark "nix packages" "search.nixos.org/packages")
            (mkBookmark "nix pr tracker" "nixpk.gs/pr-tracker.html")
            (mkBookmark "hm options" "home-manager-options.extranix.com")
            (mkBookmark "nixpkgs docs" "nixos.org/manual/nixpkgs/unstable")
            (mkBookmark "stylix docs" "stylix.danth.me")
            (mkBookmark "nixvim docs" "nix-community.github.io/nixvim")

            (mkBookmarkFolder "haskell"
              [
                (mkBookmark "haskell symbols" "github.com/takenobu-hs/haskell-symbol-search-cheatsheet")
              ])

            (mkBookmarkFolder "resources"
              [
                (mkBookmark "free icons" "feathericons.com/")
              ])

            (mkBookmarkFolder "eso"
              [
                (mkBookmark "warden build" "eso.justlootit.com/eso-builds/warden/warden-healer-eso")
              ])

            (mkBookmarkFolder "knitting"
              [
                (mkBookmark "hat" "bhookedcrochet.com/2022/03/18/everyday-rib-knit-hat")
              ])

            (mkBookmarkFolder "misc"
              [
                (mkBookmark "white page" "saygo-png.github.io/white-page")
              ])
          ];
        };
        extensions = {
          force = true;
          packages = let
            addons = inputs.firefox-addons.packages.${system};
          in [
            addons.sponsorblock
            addons.ublock-origin
            addons.gruvbox-dark-theme
            addons.close-tabs-shortcuts
          ];
          settings = let
            ublock-origin = "uBlock0@raymondhill.net";
          in {
            ${ublock-origin} = {
              force = true;
              settings = let
                customLists = [
                  "https://raw.githubusercontent.com/i5heu/ublock-hide-yt-shorts/master/list.txt"
                  "https://raw.githubusercontent.com/DandelionSprout/adfilt/master/BrowseWebsitesWithoutLoggingIn.txt"
                ];
              in {
                importedLists = customLists;
                externalLists = lib.concatStringsSep "\n" customLists;
                selectedFilterLists =
                  [
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
                    "ublock-experimental"
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
                  ]
                  ++ customLists;
              };
            };
          };
        };
        search = {
          # {{{
          enable = true;
          force = true;
          default = "google";
          engines = let
            mkEngine = definedAliases: template: {
              inherit definedAliases;
              urls = [{inherit template;}];
            };
          in {
            bing.metadata.hidden = true;
            google = mkEngine [] "https://www.google.com/search?q={searchTerms}";
            ddg = mkEngine [] "https://duckduckgo.com/?q={searchTerms}";
            "NixOS Packages" = mkEngine ["!np"] "https://search.nixos.org/packages?channel=unstable&query={searchTerms}";
            "NixOS Options" = mkEngine ["!no"] "https://search.nixos.org/options?channel=unstable&query={searchTerms}";
            "Noogle.dev" = mkEngine ["!ng" "!noogle"] "https://noogle.dev/q?term={searchTerms}";
            "Home Manager Options" = mkEngine ["!hm"] "https://home-manager-options.extranix.com/?query={searchTerms}";
          };
        }; # }}}
      };
    };
  };
}
