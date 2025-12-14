{
  lib,
  pkgs,
  config,
  inputs,
  system,
  conHome,
  conHost,
  conFlakePath,
  ...
}: {
  environment.systemPackages = [
    (pkgs.stdenv.mkDerivation {
      pname = "hordes-kiosk";
      version = "1.0.0";

      src = pkgs.writeText "hordes-kiosk.sh" ''
        #!/bin/bash
        exec ${lib.getExe pkgs.ungoogled-chromium} \
          --no-first-run \
          --disable-infobars \
          --disable-session-crashed-bubble \
          --disable-suggest-tab \
          --disable-translate \
          --disable-features=TranslateUI \
          --no-default-browser-check \
          --disable-software-rasterizer \
          --disable-background-timer-throttling \
          --disable-renderer-backgrounding \
          --disable-backgrounding-occluded-windows \
          --disable-features=VizDisplayCompositor \
          --app=https://hordes.io
      '';

      dontUnpack = true;

      nativeBuildInputs = [pkgs.makeWrapper];

      installPhase = ''
        runHook preInstall

        # Install the script
        mkdir -p $out/bin
        cp $src $out/bin/hordes-kiosk
        chmod +x $out/bin/hordes-kiosk

        # Install desktop file
        mkdir -p $out/share/applications
        cat > $out/share/applications/hordes-kiosk.desktop << EOF
        [Desktop Entry]
        Type=Application
        Name=Hordes.io Kiosk
        Comment=Launch Hordes.io in fullscreen kiosk mode
        Exec=$out/bin/hordes-kiosk
        Terminal=false
        Categories=Game;Network;
        StartupNotify=true
        MimeType=x-scheme-handler/http;x-scheme-handler/https;
        EOF

        runHook postInstall
      '';

      meta = with pkgs.lib; {
        description = "Hordes.io game launcher in chromium kiosk mode";
        homepage = "https://hordes.io";
        license = licenses.mit;
        platforms = platforms.linux;
      };
    })

    (pkgs.callPackage (lib.my.relativeToRoot "resources/saygo-utils")
      {niceHaskell = inputs.niceHaskell.outputs.niceHaskell.${system};})

    # Shell {{{
    (pkgs.writeScriptBin "nr"
      ''
        #!${lib.getExe pkgs.zsh}

        # Parametrized alias.
        # $@ is an array of all arguments quoted, (w) operates on words,
        # ":1:1" offsets the array by 1, and limits the range to 1 , ":2" offsets the array

        nix run "nixpkgs#''${(w)@:1:1}" -- ''${(w)@:2}
      '')

    (pkgs.writeScriptBin "snr"
      ''
        #!${lib.getExe pkgs.zsh}

        sudo nix run "nixpkgs#''${(w)@:1:1}" -- ''${(w)@:2}
      '')

    (pkgs.writeShellApplication {
      name = "nrepl";
      runtimeInputs = with pkgs; [coreutils];
      text = ''
        if [[ -f repl.nix ]]; then
          nix repl --arg host '"${conHost}"' --file ./repl.nix "$@"
        elif [[ -f ./flake.nix ]]; then
          nix repl .
        else
          nix repl --arg host '"${conHost}"' --file ${conFlakePath}/repl.nix "$@"
        fi
      '';
    })

    (pkgs.writeShellApplication {
      name = "format-udf";
      runtimeInputs = with pkgs; [coreutils udftools];
      checkPhase = ""; # Dont shellcheck
      bashOptions = []; # Dont add extra options
      text = builtins.readFile "${inputs.format-udf}/format-udf.sh";
    })

    (pkgs.writeShellApplication {
      name = "vmrss";
      runtimeInputs = with pkgs; [coreutils bc];
      checkPhase = ""; # Dont shellcheck
      bashOptions = []; # Dont add extra options
      text = builtins.readFile "${inputs.vmrss}/vmrss";
    })

    (pkgs.writeShellApplication {
      name = "airplane-mode";
      runtimeInputs = [pkgs.util-linux];
      text = ''
        [ "$#" -ne 1 ] && { echo "Usage: $0 {on|off}"; exit 1; }
        case "$1" in
          on)
            # block all wireless devices.
            if rfkill block all; then
              echo "Airplane mode enabled successfully."
              else
              echo "Failed to enable airplane mode."
            fi
            ;;
          off)
            # unblock all wireless devices.
            if rfkill unblock all; then
              echo "Airplane mode disabled successfully."
              else
              echo "Failed to disable airplane mode."
            fi
            ;;
          *)
            echo "Invalid argument: $1"
            echo "Usage: $0 {on|off}"
            exit 1
            ;;
        esac
      '';
    })

    (pkgs.writeShellApplication {
      name = "fzfcd";
      runtimeInputs = with pkgs; [fzf fd coreutils eza];
      text = ''
        dir=$(fd --hidden --type directory --type file --maxdepth 15 . | fzf --preview "eza --tree --color=always {}")

        if [ -z "$dir" ]; then
         exit 1
        fi

        if [ -d "$dir" ]; then
            printf '%s\n' "$dir"
        elif [ -f "$dir" ]; then
            dirname "$dir"
        else
            exit 1
        fi
      '';
    })

    (pkgs.writeShellApplication {
      name = "pasteimg";
      runtimeInputs = with pkgs; [xclip coreutils];
      text = ''
        xclip -selection clipboard -t image/png -o | tee "$1" >/dev/null
      '';
    })

    (pkgs.writeShellApplication {
      name = "myAutostart.sh";
      runtimeInputs = with pkgs; [xorg.xrandr kdePackages.polkit-kde-agent-1 xmousepasteblock xssproxy];
      text = ''
        run() {
          if ! pgrep -f "$1" ;
          then
            "$@"&
          fi
        }
        xrandr -r ${toString config.const.refreshRate}
        run "polkit-kde-authentication-agent-1" &
        run "xmousepasteblock" &
        run "xssproxy" &
        run "udiskie" &
        run "remaps" &
        run "$TERMINAL" &
      '';
    })

    (pkgs.writeShellApplication {
      name = "tree";
      runtimeInputs = [pkgs.eza];
      text = ''
        eza --group-directories-first --tree
      '';
    })

    (pkgs.writeShellApplication {
      name = "remaps";
      runtimeInputs = with pkgs; [coreutils xdotool xcape xorg.setxkbmap xorg.xset];
      text = ''
        # This script is called on startup to remap keys.
        # Decrease key repeat delay and increase key repeat rate.
        xset r rate ${toString config.services.xserver.autoRepeatDelay} ${toString config.services.xserver.autoRepeatInterval}
        # Turn off caps lock if on since there is no longer a key for it.
        xset -q | grep -q "Caps Lock:\s*on" && xdotool key Caps_Lock
        # Disable touchpad
        ${lib.getExe pkgs.xorg.xinput} disable 'SynPS/2 Synaptics TouchPad'
      '';
    })

    (pkgs.writeShellApplication {
      name = "rdepends";
      runtimeInputs = [pkgs.nix];
      text = ''
        if [ "$#" -eq 0 ]; then
            echo "No package(s) provided."
            exit 1
        fi

        parent="/run/current-system"
        child="\$(nix eval --raw \"nixpkgs#$1.outPath\")"

        if [ "$#" -eq 2 ]; then
        parent="\$(nix eval --raw \"nixpkgs#$1.outPath\")"
        child="\$(nix eval --raw \"nixpkgs#$2.outPath\")"
        fi

        # echo then run the command
        cmd="nix why-depends \"$parent\" \"$child\""
        echo "$cmd" >&2
        eval "$cmd"
      '';
    })

    (pkgs.writeShellScriptBin "hwinfolist"
      ''
        echo "GPU"
        echo "nr amdgpu_top --gui"
        echo "General Tree"
        echo "snr lshw -json | nvim -c 'set filetype=json'"
      '')

    (pkgs.writeShellScriptBin "nix-clean-hard"
      ''
        nix-env --delete-generations 3d
        sudo nix-env --delete-generations 3d

        nix-collect-garbage --delete-older-than 3d
        sudo nix-collect-garbage --delete-older-than 3d

        nix-collect-garbage -d
        sudo nix-collect-garbage -d

        nix-store --gc
        sudo nix-store --gc

        nix store optimise
        sudo nix store optimise

        rm -rf ${conHome}/.local/state/home-manager
        rm -f ${conHome}/.local/state/nix/profiles/home-manager*
      '')

    (let
      connection-test =
        pkgs.writeShellScriptBin "connection-test"
        ''
          connected_to_internet() {
            test_urls="\
            https://www.google.com/ \
            https://www.duck.com/ \
            https://www.cloudflare.com/ \
            "
            processes="0"
            pids=""
            for test_url in $test_urls; do
              curl --silent --head "$test_url" > /dev/null &
              pids="$pids $!"
              processes=$(($processes + 1))
            done
            while [ $processes -gt 0 ]; do
              for pid in $pids; do
                if ! ps | grep "^[[:blank:]]*$pid[[:blank:]]" > /dev/null; then
                  # Process no longer running
                  processes=$(($processes - 1))
                  pids=$(echo "$pids" | sed --regexp-extended "s/(^| )$pid($| )/ /g")

                  if wait $pid; then
                    # Success! We have a connection to at least one public site, so the
                    # internet is up. Ignore other exit statuses.
                    kill -TERM $pids > /dev/null 2>&1 || true
                    wait $pids
                    return 0
                  fi
                fi
              done
              wait -n $pids # Better than sleep, but not supported on all systems
            done
            return 1
          }
          if connected_to_internet; then
              echo "Connected to internet" && ${lib.getExe pkgs.libnotify} "Connected to internet"
            else
              echo "No internet connection"
          fi
        '';
    in
      pkgs.writeShellApplication {
        name = "connection-tester";
        runtimeInputs = with pkgs; [coreutils];
        text = ''
          watch -n 0.5 ${lib.getExe connection-test}
        '';
      })

    (pkgs.writeShellApplication {
      name = "mynix-list-packages";
      runtimeInputs = with pkgs; [coreutils fzf];
      text = ''
        nix-store --query --requisites /run/current-system | cut -d- -f2- | sort | uniq
      '';
    })

    (pkgs.writeShellApplication {
      name = "hyprcorder.sh";
      bashOptions = ["pipefail"];
      runtimeInputs = with pkgs; [wl-screenrec slurp ripdrag libnotify coreutils procps];
      text = ''
        notify() {
          notify-send "$@"
          echo "$@"
        }
        dir="${conHome}/Pictures/screencaptures"
        [ -d "$dir" ] || mkdir -p "$dir"
        filename="$dir/$(date +%y.%m.%d-%H:%M).mp4"

        if pgrep wl-screenrec &>/dev/null; then
          kill -s SIGINT $(pgrep wl-screenrec) && notify "wl-screenrec stopped"
          pushd "$dir" || exit 2
          ripdrag "$(find . -maxdepth 1 -type f -printf '%T@\t%Tc %6k Ki\t%P\n' | sort -n | cut -f 3- | tail -n1)"
          popd || exit 2
          exit 0
        fi

        if [ $# -eq 0 ]; then
          wl-screenrec -b "3 MB" -m 24 -f "$filename" --audio &
        else
          dim="$(slurp)"
          if [ -z "$dim" ]; then
            notify "No area selected"
            exit 2
          fi
          wl-screenrec -b "3 MB" -m 24 -f "$filename" -g "$dim" &
        fi

        if pgrep wl-screenrec &>/dev/null; then
          notify "wl-screenrec started"
        else
          notify "wl-screenrec failed to start"
        fi
      '';
    })
    # }}}
  ];
}
