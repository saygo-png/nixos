{
  pkgs,
  conUsername,
  inputs,
  ...
}: {
  home-manager.users.${conUsername} = {
    lib,
    config,
    osConfig,
    ...
  }: {
    home = {
      packages = with pkgs; let
        packages = osConfig.const.libs;
        myShell-nix = pkgs.writeText "shell.nix" ''
          {pkgs ? import <nixpkgs> {}}:
          pkgs.mkShell {
            packages = with pkgs; [
              ${lib.concatMapStringsSep "\n" (package: "${package}") packages}
            ];
          }
        '';
      in [
        (writeShellApplication
          {
            name = "myShell";
            runtimeInputs = [];
            text = ''
              nix-shell ${myShell-nix}
            '';
          })

        (writeShellApplication {
          name = "update_mutable.sh";
          runtimeInputs = [coreutils];
          text = ''
            set -o pipefail
            set -u
            IFS= # don't split
            set +f # do glob

            KRITAHOME="${config.home.homeDirectory}/.local/share/krita"
            KRITANIXHOME="${osConfig.const.extrasNixosPath}/krita"

            ANKIHOME="${config.home.homeDirectory}/.local/share/Anki2"
            ANKINIXHOME="${osConfig.const.extrasNixosPath}/anki"

            # Krita
            rm -vrf "''${KRITANIXHOME:?}/*"
            cp -vrf "$KRITAHOME/." "$KRITANIXHOME/krita-toplevel"
            cp -vf "$HOME/.config/kritarc" "$KRITANIXHOME/kritarc"
            cp -vf "$HOME/.config/kritadisplayrc" "$KRITANIXHOME/kritadisplayrc"
            # Don't include the cache
            rm -vf "$KRITANIXHOME/krita-toplevel/resourcecache.sqlite"

            cd "$KRITANIXHOME"/.. || exit
            git add -A
            git commit -m "chore: auto commit update"
            git push

            # Anki
            rm -vrf "''${ANKINIXHOME:?}/*"
            cp -vrf "$ANKIHOME"/addons* "$ANKINIXHOME"/.
            # Don't include the cache
            find "$ANKINIXHOME" -type d -name "__pycache__" -print0 | xargs -0 rm -vrf

            cd "$ANKINIXHOME"/.. || exit
            git add -A
            git commit -m "chore: auto commit update"
            git push
          '';
        })
      ];
      # This allows for semi-declarative configuration.
      # However it makes you lag when rebuilding.
      activation.configure-krita = lib.hm.dag.entryAfter ["writeBoundary"] ''
        run mkdir -p "${config.xdg.configHome}"

        run mkdir -p "${config.home.homeDirectory}/.local/share/krita"
        run chmod -R $VERBOSE_ARG u+w,g+w "${config.home.homeDirectory}/.local/share/krita"
        run cp -rf $VERBOSE_ARG "${inputs.extras-nixos}/krita/kritarc" "${config.xdg.configHome}/kritarc"
        run cp -rf $VERBOSE_ARG "${inputs.extras-nixos}/krita/kritadisplayrc" "${config.xdg.configHome}/kritadisplayrc"
        run cp -rf $VERBOSE_ARG "${inputs.extras-nixos}/krita/krita-toplevel"/. "${config.home.homeDirectory}/.local/share/krita"
        run chmod -R $VERBOSE_ARG u+w,g+w "${config.home.homeDirectory}/.local/share/krita"

        run mkdir -p "${config.home.homeDirectory}/.local/share/Anki2"
        run chmod -R $VERBOSE_ARG u+w,g+w "${config.home.homeDirectory}/.local/share/Anki2"
        run cp -rf $VERBOSE_ARG "${inputs.extras-nixos}/anki"/. "${config.home.homeDirectory}/.local/share/Anki2"
        run chmod -R $VERBOSE_ARG u+w,g+w "${config.home.homeDirectory}/.local/share/Anki2"
      '';
    };
  };
}
