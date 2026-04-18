{
  lib,
  pkgs,
  config,
  conUsername,
  inputs,
  system,
  conHome,
  ...
}: let
  inherit (import (lib.my.relativeToRoot "modules/waybar/lib.nix") lib pkgs) wrapWaybarWithConfig;
in {
  imports = lib.my.withModules ["waylandBase.nix"];

  nixpkgs.overlays = [
    (_: prev: {
      niri = let
        niriBase = prev.niri.overrideAttrs (old: {
          patches =
            (old.patches or [])
            ++ [
              (lib.my.relativeToRoot "resources/niri/transparent-fullscreen-pr.patch")
              (lib.my.relativeToRoot "resources/niri/dynamic-zoom.patch")
            ];
        });
      in
        prev.symlinkJoin {
          name = "niri-patched-session";
          paths = [niriBase];
          postBuild = ''
            sed -i "2a . /etc/profiles/per-user/${conUsername}/etc/profile.d/hm-session-vars.sh" "$out/bin/niri-session"
          '';
        };
    })
  ];

  environment.systemPackages = let
    waybar-config =
      lib.attrsets.recursiveUpdate
      config.const.waybarBase
      {
        mainBar = {
          layer = "top";
          modules-left = ["niri/workspaces" "niri/window"];
        };
      };

    waybar-niri = wrapWaybarWithConfig waybar-config "niri";
  in [
    waybar-niri
    pkgs.xwayland-satellite
  ];

  xdg = {
    portal = {
      enable = true;
      extraPortals = [
        pkgs.xdg-desktop-portal-gtk
        pkgs.xdg-desktop-portal-gnome
      ];
      config = {
        niri = {
          default = "gnome;gtk";
          "org.freedesktop.impl.portal.Access" = "gtk";
          "org.freedesktop.impl.portal.Notification" = "gtk";
          "org.freedesktop.impl.portal.Secret" = "gnome-keyring";

          "org.freedesktop.impl.portal.FileChooser" = "gtk";
        };
      };
      configPackages = [pkgs.niri]; # This gets overridden by the config above
    };
  };

  services.gnome.gnome-keyring.enable = true;
  systemd.user.services = let
    mkNiriService = {
      ExecStart,
      description,
    }: {
      inherit description;
      wantedBy = ["niri.service"];
      partOf = ["graphical-session.target"];
      after = ["graphical-session.target"];
      serviceConfig = {
        Type = "simple";
        inherit ExecStart;
        Restart = "on-failure";
        RestartSec = "3s";
        TimeoutStopSec = 10;
      };
    };
  in {
    niri-flake-polkit = mkNiriService {
      description = "PolicyKit Authentication Agent for Niri";
      ExecStart = "${pkgs.kdePackages.polkit-kde-agent-1}/libexec/polkit-kde-authentication-agent-1";
    };
    saywallpaper = mkNiriService {
      description = "Saywallpaper wallpaper daemon for Niri";
      ExecStart = "${lib.getExe inputs.saywallpaper.packages.${system}.saywallpaper} -i ${conHome}/.config/wallpaper.raw";
    };
    saybar = mkNiriService {
      description = "Saybar status bar for Niri";
      ExecStart = "${conHome}/.local/bin/saybar";
    };
  };

  programs.niri.enable = true;

  home-manager.users.${conUsername} = _: {
    services.polkit-gnome.enable = true;
    services.swayosd.enable = true;
    xdg.configFile."niri/config.kdl".source = lib.my.relativeToRoot "resources/niri/niri.kdl";
  };
}
