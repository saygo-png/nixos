{
  lib,
  pkgs,
  self,
  conUsername,
  inputs,
  system,
  conHome,
  ...
}: {
  imports = lib.my.withModules ["waylandBase.nix"];

  programs.niri = {
    enable = true;
    useNautilus = false;
  };

  nixpkgs.overlays = [
    (_: prev: {
      niri = prev.niri.overrideAttrs (old: {
        patches =
          (old.patches or [])
          ++ [
            (lib.my.relativeToRoot "resources/niri/transparent-fullscreen-pr.patch")
            (lib.my.relativeToRoot "resources/niri/float-over-fullscreen.patch")
            # (lib.my.relativeToRoot "resources/niri/dynamic-zoom.patch")
          ];
        postInstall =
          (old.postInstall or "")
          + ''
            sed -i "2a . /etc/profiles/per-user/${conUsername}/etc/profile.d/hm-session-vars.sh" "$out/bin/niri-session"
          '';
      });
    })
  ];

  environment.systemPackages = [pkgs.xwayland-satellite];

  systemd.user = {
    services = let
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
      saywallpaper =
        mkNiriService {
          description = "Saywallpaper wallpaper daemon for Niri";
          ExecStart = "${lib.getExe inputs.saywallpaper.packages.${system}.saywallpaper} -i ${conHome}/.config/wallpaper.raw";
        }
        // {
          environment.WAYLAND_DEBUG = "1";
        };
      saybar =
        mkNiriService {
          description = "Saybar status bar for Niri";
          ExecStart = "${conHome}/.local/bin/saybar";
        }
        // {
          path = [self.packages.${system}.saygo-utils];
          environment.WAYLAND_DEBUG = "1";
        };
    };
  };

  home-manager.users.${conUsername} = _: {
    services.polkit-gnome.enable = true;
    services.swayosd.enable = true;
    xdg.configFile."niri/config.kdl".source = lib.my.relativeToRoot "resources/niri/niri.kdl";
  };
}
