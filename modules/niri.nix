{
  lib,
  pkgs,
  config,
  conUsername,
  ...
}: let
  inherit (import (lib.my.relativeToRoot "modules/waybar/lib.nix") lib pkgs) wrapWaybarWithConfig;
in {
  imports = lib.my.withModules ["waylandBase.nix"];

  nixpkgs.overlays = [
    (_: prev: {
      niri = prev.niri.overrideAttrs (old: {
        patches =
          (old.patches or [])
          ++ [
            (lib.my.relativeToRoot "resources/niri/transparent-fullscreen-pr.patch")
            (lib.my.relativeToRoot "resources/niri/dynamic-zoom.patch")
          ];
      });
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
  systemd.user.services.niri-flake-polkit = {
    description = "PolicyKit Authentication Agent for Niri";
    wantedBy = ["niri.service"];
    after = ["graphical-session.target"];
    partOf = ["graphical-session.target"];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.kdePackages.polkit-kde-agent-1}/libexec/polkit-kde-authentication-agent-1";
      Restart = "on-failure";
      RestartSec = 1;
      TimeoutStopSec = 10;
    };
  };

  programs.niri.enable = true;

  home-manager.users.${conUsername} = _: {
    services.polkit-gnome.enable = true;
    xdg.configFile."niri/config.kdl".source = lib.my.relativeToRoot "resources/niri/niri.kdl";
  };
}
