{
  lib,
  pkgs,
  config,
  conUsername,
  ...
}: let
  inherit (import (lib.my.relativeToRoot "modules/waybar/lib.nix") lib pkgs) wrapWaybarWithConfig;
in {
  imports = lib.my.withModules ["myWaylandBase.nix"];

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
    pkgs.swaybg
    pkgs.xwayland-satellite
    (pkgs.callPackage (lib.my.relativeToRoot "resources/haskell/niri-switch-keyboard-rofi") {})
  ];

  programs.niri.enable = true;

  home-manager.users.${conUsername} = _: {
    services.polkit-gnome.enable = true;
    xdg.configFile."niri/config.kdl".source = lib.my.relativeToRoot "resources/niri.kdl";
  };
}
