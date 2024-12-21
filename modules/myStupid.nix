{
  pkgs,
  conHome,
  conUsername,
  ...
}: {
  environment.systemPackages = with pkgs; [
    # (writeShellApplication {
    #   name = "update-secrets.sh";
    #   runtimeInputs = [coreutils];
    #   text = ''
    #     key=$(cat ${conHome}/Sync/secrets/openai.txt)
    #     echo "$key"
    #     export OPENAI_API_KEY="$key"
    #     echo "$OPENAI_API_KEY"
    #   '';
    # })
  ];
  home-manager.users.${conUsername} = {
    lib,
    config,
    ...
  }: {
    # home.packages = with pkgs; [
    # ];
    home = {
      packages = with pkgs; [
        (writeShellApplication {
          name = "update_mutable.sh";
          runtimeInputs = [coreutils];
          text = ''
            set -o pipefail
            set -u
            IFS= # don't split
            set +f # do glob

            KRITAHOME="${conHome}/.local/share/krita"
            KRITANIXHOME="${conFlakePath}/resources/krita"
            rm -vrf "''${KRITANIXHOME:?}/*"
            cp -vrf "$KRITAHOME/." "$KRITANIXHOME/krita-toplevel"
            cp -vf "$HOME/.config/kritarc" "$KRITANIXHOME/kritarc"
            cp -vf "$HOME/.config/kritadisplayrc" "$KRITANIXHOME/kritadisplayrc"
            # Dont include the cache
            rm -vf "$KRITANIXHOME/krita-toplevel/resourcecache.sqlite"

            ANKIHOME="${conHome}/.local/share/Anki2"
            ANKINIXHOME="${conFlakePath}/resources/anki"
            rm -vrf "''${ANKINIXHOME:?}/*"
            cp -vrf "$ANKIHOME"/addons* "$ANKINIXHOME"/.
            find "$ANKINIXHOME" -type d -name "__pycache__" -print0 | xargs -0 rm -vrf
          '';
        })
      ];
      # This allows for semi-declarative configuration.
      # However it makes you lag when rebuilding.
      activation.configure-krita = lib.hm.dag.entryAfter ["writeBoundary"] ''
        run mkdir -p "${config.xdg.configHome}"

        run mkdir -p "${config.home.homeDirectory}/.local/share/krita"
        run chmod -R $VERBOSE_ARG u+w,g+w "${config.home.homeDirectory}/.local/share/krita"
        run cp -rf $VERBOSE_ARG "${builtins.toPath ./resources/krita/kritarc}" "${config.xdg.configHome}/kritarc"
        run cp -rf $VERBOSE_ARG "${builtins.toPath ./resources/krita/kritadisplayrc}" "${config.xdg.configHome}/kritadisplayrc"
        run cp -rf $VERBOSE_ARG "${builtins.toPath ./resources/krita/krita-toplevel}"/. "${config.home.homeDirectory}/.local/share/krita"
        run chmod -R $VERBOSE_ARG u+w,g+w "${config.home.homeDirectory}/.local/share/krita"

        run mkdir -p "${config.home.homeDirectory}/.local/share/Anki2"
        run chmod -R $VERBOSE_ARG u+w,g+w "${config.home.homeDirectory}/.local/share/Anki2"
        run cp -rf $VERBOSE_ARG "${builtins.toPath ./resources/anki}"/. "${config.home.homeDirectory}/.local/share/Anki2"
        run chmod -R $VERBOSE_ARG u+w,g+w "${config.home.homeDirectory}/.local/share/Anki2"
      '';
    };
  };
}
