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
      packages = with pkgs; [
        (writeShellApplication {
          name = "update_mutable.sh";
          runtimeInputs = [coreutils];
          text = ''
            set -u
            IFS= # don't split
            set +f # do glob

            ANKIHOME="${config.home.homeDirectory}/.local/share/Anki2"
            ANKINIXHOME="${osConfig.const.extrasNixosPath}/anki"

            # Anki
            rm -vrf "''${ANKINIXHOME:?}"/*
            cp -vrf "$ANKIHOME"/addons* "$ANKINIXHOME"/.
            # Don't include the cache
            find "$ANKINIXHOME" -type d -name "__pycache__" -print0 | xargs -0 rm -vrf

            cd "$ANKINIXHOME"/.. || exit
            git add -A || true
            git commit -m "chore: auto commit update" || true
            git push || true
          '';
        })
      ];
      # This allows for semi-declarative configuration.
      # However it makes you lag when rebuilding.
      activation.configure-krita = lib.hm.dag.entryAfter ["writeBoundary"] ''
        run mkdir -p "${config.xdg.configHome}"

        run mkdir -p "${config.home.homeDirectory}/.local/share/Anki2"
        run chmod -R $VERBOSE_ARG u+w,g+w "${config.home.homeDirectory}/.local/share/Anki2"
        run cp -rf $VERBOSE_ARG "${inputs.extras-nixos}/anki"/. "${config.home.homeDirectory}/.local/share/Anki2"
        run chmod -R $VERBOSE_ARG u+w,g+w "${config.home.homeDirectory}/.local/share/Anki2"
      '';
    };
  };
}
