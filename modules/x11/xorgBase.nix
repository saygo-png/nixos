{
  lib,
  pkgs,
  config,
  conUsername,
  ...
}: {
  imports =
    [
      ({config, ...}: {
        options = {
          const = config.constLib.mkConstsFromSet {
            xinitBase =
              # bash
              ''
                if test -z "$DBUS_SESSION_BUS_ADDRESS"; then
                  eval $(dbus-launch --exit-with-session --sh-syntax)
                fi
                systemctl --user import-environment DISPLAY XAUTHORITY XDG_CURRENT_DESKTOP
                if command -v dbus-update-activation-environment >/dev/null 2>&1; then
                  dbus-update-activation-environment DISPLAY XAUTHORITY XDG_CURRENT_DESKTOP
                fi
                systemctl --user import-environment PATH &
                dbus-update-activation-environment --systemd PATH &
                hash dbus-update-activation-environment 2>/dev/null &

                export XDG_SESSION_TYPE=x11
                picom &
              '';
          };
        };
      })
    ]
    ++ lib.my.withModules [
      "x11/picom.nix"
      "flameshot.nix"
    ];

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Polkit (needed for window managers)
  security.polkit.enable = lib.mkDefault true;

  # NixOS is retarded and turns on lightdm by default.
  services.xserver.displayManager = lib.mkDefault {
    startx.enable = true;
    sx.enable = true;
    lightdm.enable = false;
  };

  # We make sx use a wrapped xorg, since it does not parse xserverrc like startx, making it
  # not use a lot of nix options like extraLayouts in xkb.
  nixpkgs.overlays = [
    (final: prev: {
      xorgWrapperForSx = prev.writeShellScriptBin "Xorg" ''
        exec ${prev.xorgserver}/bin/X ${toString config.services.xserver.displayManager.xserverArgs} "$@"
      '';

      sx = prev.sx.overrideAttrs (_oldAttrs: {
        postInstall = ''
          patsh -p "${final.xorgWrapperForSx}/bin:$PATH" -f $out/bin/sx -s ${builtins.storeDir}

          install -Dm755 -t $out/share/xsessions ${
            prev.makeDesktopItem {
              name = "sx";
              desktopName = "sx";
              comment = "Start a xorg server";
              exec = "sx";
            }
          }/share/applications/sx.desktop
        '';
      });
    })
  ];

  environment.systemPackages = with pkgs; [
    xclip # Xorg wl-clipboard
    flameshot # X11 screenshot tool
    picom # Compositor
  ];

  home-manager.users.${conUsername} = {config, ...}: {
    home = {
      file =
        lib.mkIf ("${config.xresources.path}".source or false)
        {
          # auto xrdb
          ${config.xresources.path}.onChange = ''
            [[ -z "$\{DISPLAY:-}" ]] && echo Display not set && exit 0
            run ${lib.getExe pkgs.xorg.xrdb} "${config.xresources.path}"
          '';
        };
    };
  };
}
