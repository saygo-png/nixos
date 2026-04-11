{
  conUsername,
  pkgs,
  lib,
  ...
}: {
  environment.systemPackages = [
    (pkgs.stdenv.mkDerivation {
      pname = "VIA-kiosk";
      version = "1.0.0";

      src = let
        args = [
          "--password-store=basic" # Hide stupid keyring popup
          "--no-first-run"
          "--disable-infobars"
          "--disable-suggest-tab"
          "--disable-translate"
          "--disable-features=TranslateUI"
          "--no-default-browser-check"
          "--app=https://usevia.app"
        ];
      in
        pkgs.writeText "VIA-kiosk.sh" ''
          #!/bin/bash
          exec ${lib.getExe pkgs.ungoogled-chromium} ${builtins.concatStringsSep " " args}
        '';

      dontUnpack = true;

      nativeBuildInputs = [pkgs.makeWrapper];

      installPhase = ''
        runHook preInstall

        # Install the script
        mkdir -p $out/bin
        cp $src $out/bin/VIA-kiosk
        chmod +x $out/bin/VIA-kiosk

        # Install desktop file
        mkdir -p $out/share/applications
        cat > $out/share/applications/VIA-kiosk.desktop << EOF
        [Desktop Entry]
        Type=Application
        Name=VIA configurator kiosk
        Comment=Launch the VIA app in fullscreen kiosk mode
        Exec=$out/bin/VIA-kiosk
        Terminal=false
        StartupNotify=true
        MimeType=x-scheme-handler/http;x-scheme-handler/https;
        EOF
        runHook postInstall
      '';

      meta = with pkgs.lib; {
        description = "VIA app launcher in chromium kiosk mode";
        homepage = "https://usevia.app/";
        license = licenses.mit;
        platforms = platforms.linux;
      };
    })
  ];

  # This lets the VIA webapp find the keyboard.
  services.udev = {
    extraRules = ''
      KERNEL=="hidraw*", SUBSYSTEM=="hidraw", GROUP="hidraw", MODE="0660", TAG+="uaccess", TAG+="udev-acl"
    '';
  };
  users.groups.hidraw = {};
  users.users.${conUsername}.extraGroups = ["hidraw"];
}
