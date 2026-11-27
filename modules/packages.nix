{
  lib,
  self,
  pkgs,
  config,
  system,
  conHost,
  conFlakePath,
  mkLazyDesktopItem,
  ...
}: {
  custom.lazyPackages = [
    (lib.my.mkChromiumKiosk pkgs {
      name = "hordes-kiosk";
      url = "https://hordes.io";
    })
  ];

  environment.systemPackages = [
    (mkLazyDesktopItem {
      name = "hordes-kiosk";
      desktopName = "Hordes.io Kiosk";
      comment = "Launch Hordes.io in fullscreen kiosk mode";
      categories = ["Game" "Network"];
      exec = "comma-bg hordes-kiosk";
    })

    (self.packages.${system}.saygo-utils.override {
      inherit (config.custom) lazyPackages;
      # Not using `self` here to avoid rebuilds on all changes.
      lazyInstallable = name: "/etc/flake-src#lazyPackages.${conHost}.${name}";
    })

    (pkgs.writeShellApplication {
      name = "nrepl";
      runtimeInputs = with pkgs; [coreutils];
      text = ''
        if [[ -f repl.nix ]]; then
          nix repl --arg host '"${conHost}"' --file ./repl.nix "$@"
        elif [[ -f ./nix-meta/repl.nix ]]; then
          nix repl --arg host '"${conHost}"' --file ./nix-meta/repl.nix "$@"
        elif [[ -f ./flake.nix ]]; then
          nix repl .
        else
          nix repl --arg host '"${conHost}"' --file ${conFlakePath}/repl.nix "$@"
        fi
      '';
    })

    (let
      connection-test = pkgs.writeShellApplication {
        name = "connection-test";
        runtimeInputs = [pkgs.curl pkgs.libnotify];
        text = ''
          for url in https://www.google.com/ https://www.duck.com/ https://www.cloudflare.com/; do
            curl --silent --head "$url" > /dev/null &
          done
          # Connected as soon as one of them answers.
          for _ in 1 2 3; do
            if wait -n; then
              kill %1 %2 %3 2> /dev/null || true
              echo "Connected to internet"
              notify-send "Connected to internet"
              exit
            fi
          done
          echo "No internet connection"
        '';
      };
    in
      pkgs.writeShellApplication {
        name = "connection-tester";
        runtimeInputs = [pkgs.procps];
        text = ''
          watch -n 0.5 ${lib.getExe connection-test}
        '';
      })
  ];
}
