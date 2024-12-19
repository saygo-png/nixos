{
  lib,
  pkgs,
  config,
  conHome,
  conUsername,
  conFlakePath,
  conFlakePathRel,
  ...
}: {
  environment.systemPackages = with pkgs; [
    (pkgs.callPackage "${conFlakePathRel}/resources/librewolf-bin.nix" {}) # Browser, bin is from appimage and fixes discord crash

    (writeShellScriptBin "hyprland-next-visible-client.sh"
      (builtins.readFile "${conFlakePathRel}/resources/scripts/hyprland-next-visible-client.sh"))

    (writeShellScriptBin "vmrss"
      (builtins.readFile "${conFlakePathRel}/resources/scripts/vmrss.sh"))

    (writeShellScriptBin "monitor-toggle"
      (builtins.readFile "${conFlakePathRel}/resources/scripts/monitor-toggle.sh"))

    (writers.writePython3Bin "keepfilelist"
      {
        libraries = [pkgs.python3Packages.send2trash];
        flakeIgnore = ["E265" "E225" "E111" "E305" "E501" "E121" "E302" "E114" "F541"];
      }
      (builtins.readFile "${conFlakePathRel}/resources/scripts/keepfilelist.py"))

    xdotool
    (writers.writePython3Bin "d3-autocast"
      {
        libraries = [];
        flakeIgnore = ["E265" "E225" "E111" "E305" "E501" "E121" "E302" "E114" "F541"];
      }
      (builtins.readFile "${conFlakePathRel}/resources/scripts/diablo3-autocast/d3-autocast.py"))

    (writeShellApplication {
      name = "xkb-switch-rofi";
      runtimeInputs = [coreutils xkb-switch rofi-wayland];
      text = builtins.readFile "${conFlakePathRel}/resources/scripts/xkb-switch-rofi.sh";
    })

    (writeShellApplication {
      name = "d3-autocast-menu";
      runtimeInputs = [coreutils udftools];
      text = builtins.readFile "${conFlakePathRel}/resources/scripts/diablo3-autocast/d3-autocast-menu.sh";
    })

    (writeShellApplication {
      name = "format-udf";
      runtimeInputs = [coreutils udftools];
      text = builtins.readFile "${conFlakePathRel}/resources/scripts/format-udf.sh";
    })

    (writeShellApplication {
      name = "airplane-mode";
      runtimeInputs = [util-linux];
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

    (pkgs.writers.writeHaskellBin "convertlink" {
        libraries = with pkgs; [
          haskellPackages.directory_1_3_9_0
          haskellPackages.unix_2_8_5_1
          haskellPackages.process_1_6_25_0
          haskellPackages.optparse-applicative
        ];
      }
      ''
        {-# LANGUAGE LambdaCase #-}
        import Control.Monad (when)
        import Options.Applicative
        import System.Directory
        import System.Posix.Files
        import System.Process (callProcess)
        import Text.Printf (printf)

        data Sample = Sample
          { path :: FilePath,
            write :: Bool
          }

        sample :: Parser Sample
        sample =
          Sample
            <$> argument
              str
              ( help "Target for the greeting"
                  <> metavar "SYMLINK"
                  <> help "Symlink that will be converted"
              )
            <*> switch
              ( long "write"
                  <> short 'w'
                  <> help "Give write permissions to the resulting file"
              )

        main :: IO ()
        main = convertlinkAndWrite =<< execParser opts
          where
            opts =
              info
                (sample <**> helper)
                ( fullDesc
                    <> progDesc "Convert a symlink to the file its pointing to"
                )

        convertlinkAndWrite :: Sample -> IO ()
        convertlinkAndWrite (Sample filepath giveWrite) = do
          pathIsSymbolicLink filepath >>= \case
            True -> do
              fileFromSymlink <- readSymbolicLink filepath
              _ <- callProcess "cp" ["--remove-destination", fileFromSymlink, filepath]
              printf "Converted symlink %s\n" filepath
              when giveWrite $ do
                _ <- callProcess "chmod" ["+w", filepath]
                pure ()
            False -> printf "convertlink: cannot convert %s: Not a symlink\n" filepath
      '')

    (writeShellApplication {
      name = "fzfcd";
      runtimeInputs = [fzf coreutils];
      text = ''
        dir=$(fd --type directory --maxdepth 3 . ~/Sync ~/Documents ~/Downloads ~/nixos | fzf)
        if [ -z "$dir" ]; then
         exit 1
        fi
        [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && echo "$dir"
      '';
    })

    (writeShellApplication {
      name = "myAutostartAwesome.sh";
      runtimeInputs = [xorg.xrandr polkit-kde-agent xmousepasteblock xssproxy];
      text = ''
        run() {
          if ! pgrep -f "$1" ;
          then
            "$@"&
          fi
        }
        xrandr -r ${builtins.toString config.const.refreshRate}
        run "polkit-kde-authentication-agent-1" &
        run "xmousepasteblock" &
        run "xssproxy" &
        run "udiskie" &
        run "remaps" &
        run "$TERMINAL" &
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

    (writeShellApplication {
      name = "tree";
      runtimeInputs = [eza];
      text = ''
        eza --group-directories-first --tree
      '';
    })

    (writeShellApplication {
      name = "remaps";
      runtimeInputs = [coreutils xdotool xcape xorg.setxkbmap xorg.xset];
      text = ''
        # This script is called on startup to remap keys.
        # Decrease key repeat delay and increase key repeat rate.
        xset r rate ${builtins.toString config.services.xserver.autoRepeatDelay} ${builtins.toString config.services.xserver.autoRepeatInterval}
        # Turn off caps lock if on since there is no longer a key for it.
        xset -q | grep -q "Caps Lock:\s*on" && xdotool key Caps_Lock
        # Disable touchpad
        ${lib.getExe pkgs.xorg.xinput} disable 'SynPS/2 Synaptics TouchPad'
      '';
    })

    (writeShellApplication {
      name = "flameshot_wrapper";
      runtimeInputs = [flameshot xdotool];
      text = ''
        # This script is called instead of flameshot in awesome to fix an issue
        # where calling flameshot would drop focus, even after taking a screenshot
        focusedwindow_before=$(xdotool getactivewindow)
        flameshot gui
        [ "$focusedwindow_before" = "$(xdotool getactivewindow)" ] && xdotool windowfocus "$focusedwindow_before"
      '';
    })

    (writeShellApplication {
      name = "flameshot_wrapper_ocr";
      runtimeInputs = [flameshot xdotool tesseract];
      text = ''
        # This script does what flameshot_wrapper does, and
        # copies the screenshoted text to your clipboard
        focusedwindow_before=$(xdotool getactivewindow)
        message=$(flameshot gui -r -s | tesseract --psm 12 --oem 1 -l eng+rus+fin+pol stdin stdout)
        [ "$focusedwindow_before" = "$(xdotool getactivewindow)" ] && xdotool windowfocus "$focusedwindow_before"
        notify-send "$message"
        echo "$message" | xclip -r -sel clip
      '';
    })

    (writeShellApplication {
      name = "flameshot_wrapper_ocr_trans";
      runtimeInputs = [flameshot xdotool tesseract translate-shell libnotify];
      text = ''
        # This script does what flameshot_wrapper does, and
        # translates the screenshotted text, sending a notification with the translation
        focusedwindow_before=$(xdotool getactivewindow)
        notify-send -t 3000 "$(flameshot gui -r -s | tesseract --psm 12 --oem 1 -l eng+rus+fin stdin stdout | trans -brief)"
        [ "$focusedwindow_before" = "$(xdotool getactivewindow)" ] && xdotool windowfocus "$focusedwindow_before"
      '';
    })

    (writeShellApplication {
      name = "rdepends";
      runtimeInputs = [nix];
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

    (writeShellScriptBin "hwinfolist"
      ''
        echo "GPU"
        echo "nr amdgpu_top --gui"
        echo "General Tree"
        echo "snr lshw -json | nvim -c 'set filetype=json'"
      '')

    (writeShellScriptBin "clean-nix-hard"
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

    (writeShellScriptBin "connection-tester"
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
      '')

    (writeShellApplication {
      name = "mynix-list-packages";
      runtimeInputs = with pkgs; [coreutils fzf];
      text = ''
        nix-store --query --requisites /run/current-system | cut -d- -f2- | sort | uniq
      '';
    })

    (writeShellApplication {
      name = "hyprcorder.sh";
      bashOptions = ["pipefail"];
      runtimeInputs = with pkgs; [wl-screenrec slurp ripdrag libnotify coreutils procps];
      text = ''
        notify() {
          notify-send "$@"
          echo "$@"
        }
        dir="${conHome}/screencaptures"
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
          wl-screenrec --audio -b "1 MB" -f "$filename" &
        else
          dim="$(slurp)"
          if [ -z "$dim" ]; then
            notify "No area selected"
            exit 2
          fi
          wl-screenrec -b "1 MB" -f "$filename" -g "$dim" &
        fi

        if pgrep wl-screenrec &>/dev/null; then
          notify "wl-screenrec started"
        else
          notify "wl-screenrec failed to start"
        fi
      '';
    })
  ];

  home-manager = {
    users.${conUsername} = {
      home = {
        # Binary (or not) blobs.
        sessionPath = ["${conHome}/bin"]; # Add ~/bin to path.
        file = {
          "bin/ow".source = "${conFlakePathRel}/resources/scripts/ow.py";
          "bin/nr" = {
            executable = true;
            text = ''
              #!/usr/bin/env zsh
              # Parametrized alias.
              # $@ is an array of all arguments quoted, (w) operates on words,
              # ":1:1" offsets the array by 1, and limits the range to 1 , ":2" offsets the array
              nix run "nixpkgs#''${(w)@:1:1}" -- ''${(w)@:2}
            '';
          };
          "bin/snr" = {
            executable = true;
            text = ''
              #!/usr/bin/env zsh
              sudo nix run "nixpkgs#''${(w)@:1:1}" -- ''${(w)@:2}
            '';
          };
        };
      };
    };
  };
}
