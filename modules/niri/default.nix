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
  imports = lib.my.withModules ["flameshot.nix"];

  # Saves permission info from access portals.
  custom.persist.home.directories = [".local/share/flatpak/db"];

  programs.niri = {
    enable = true;
    useNautilus = false;
  };

  nixpkgs.overlays = [
    (final: prev: {
      niri = let
        craneLib = inputs.crane.mkLib final;
        common = {
          inherit (prev.niri) pname version src buildInputs RUSTFLAGS NIRI_BUILD_COMMIT;
          CARGO_PROFILE_RELEASE_DEBUG = "0";
          nativeBuildInputs = [final.pkg-config final.rustPlatform.bindgenHook];
          # The crates nixpkgs downloaded, from the binary cache. crane's own download fetches smithay's whole git history.
          cargoVendorDir = final.runCommand "niri-vendor" {} ''
            mkdir $out
            substitute ${prev.niri.cargoDeps}/.cargo/config.toml $out/config.toml \
              --subst-var-by vendor ${prev.niri.cargoDeps}
          '';
          cargoExtraArgs = "--locked --no-default-features --features dbus,systemd,xdp-gnome-screencast";
          doCheck = false;
          # bindgen's arguments hold the -frandom-seed nixpkgs makes from the output path, and it rebuilds for niri when they change.
          preBuild = ''
            export BINDGEN_EXTRA_CLANG_ARGS=$(sed 's/-frandom-seed=[^ ]*//' <<< "$BINDGEN_EXTRA_CLANG_ARGS")
          '';
        };
      in
        craneLib.buildPackage (common
          // {
            cargoArtifacts = craneLib.buildDepsOnly (common // {buildPhaseCargoCommand = "cargoWithProfile build ${common.cargoExtraArgs}";});
            patches = [
              (lib.my.relativeToRoot "modules/niri/patches/transparent-fullscreen-pr.patch")
              (lib.my.relativeToRoot "modules/niri/patches/float-over-fullscreen.patch")
              (lib.my.relativeToRoot "modules/niri/patches/dynamic-zoom.patch")
              # Stops a held key repeating only when it is released, not on any release.
              (lib.my.relativeToRoot "modules/niri/patches/key-repeat.patch")
            ];
            inherit (prev.niri) outputs postPatch passthru meta;
            nativeBuildInputs = common.nativeBuildInputs ++ [final.installShellFiles];
            postInstall =
              prev.niri.postInstall
              + ''
                sed -i "2a . /etc/profiles/per-user/${conUsername}/etc/profile.d/hm-session-vars.sh" "$out/bin/niri-session"
                # crane does this for cargoVendorDir, which only points to them.
                removeReferencesToVendoredSources "$out" ${prev.niri.cargoDeps}
              '';
          });
    })
  ];

  environment.systemPackages = [
    pkgs.xwayland-satellite
    inputs.saywallpaper.packages.${pkgs.stdenv.hostPlatform.system}.saywallpaper
    pkgs.wl-clipboard
  ];

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
          ExecStart = lib.getExe inputs.saybar.packages.${system}.saybar;
        }
        // {
          path = [self.packages.${system}.saygo-utils];
          environment.WAYLAND_DEBUG = "1";
        };
    };
  };

  home-manager.users.${conUsername} = {
    services.polkit-gnome.enable = true;
    xdg.configFile."niri/config.kdl".source = lib.my.relativeToRoot "modules/niri/config.kdl";
  };
}
