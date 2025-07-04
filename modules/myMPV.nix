{
  lib,
  pkgs,
  inputs,
  conUsername,
  ...
}: {
  imports = [
    ({config, ...}: {
      options.const = config.constLib.mkConstsFromSet {
        importedMyMPVModule = true;
      };
    })
  ];
  home-manager.users.${conUsername} = {config, ...}: {
    home.packages = with pkgs; [
      # Dependencies for intersubs for mpv.
      (python3.withPackages (python312Packages: [
        python312Packages.six
        python312Packages.lxml
        python312Packages.httpx
        python312Packages.numpy
        python312Packages.pyqt5
        python312Packages.thttp
        python312Packages.requests
        python312Packages.beautifulsoup4
      ]))
      socat
    ];

    # InterSubs plugin install.
    xdg.configFile."mpv/scripts/interSubs.py" = {
      source = lib.my.getSafePath "${inputs.mpv-intersubs}/interSubs.py";
    };

    xdg.configFile."mpv/scripts/interSubs.lua" = {
      source = lib.my.getSafePath "${inputs.mpv-intersubs}/interSubs.lua";
    };

    xdg.configFile."mpv/scripts/interSubs_config.py" = {
      source = lib.my.relativeToRoot "resources/mpv/interSubs_config.py";
    };

    programs.mpv = {
      enable = true;
      bindings = {
        l = "seek 20";
        h = "seek -20";
        "]" = "add speed 0.1";
        "[" = "add speed -0.1";
        j = "seek -4";
        k = "seek 4";
        K = "cycle sub";
        J = "cycle sub down";
        w = "add sub-pos -10"; # move subtitles up
        W = "add sub-pos -1"; # move subtitles up
        e = "add sub-pos +10"; # move subtitles down
        E = "add sub-pos +1"; # move subtitles down
        "=" = "add sub-scale +0.1";
        "-" = "add sub-scale -0.1";
      };

      config = {
        speed = 1;
        hwdec = true;
        sub-pos = 90;
        keep-open = true;
        sub-auto = "all";
        sub-font-size = 40;
        sub-border-size = 2;
        sub-shadow-offset = 2;
        sub-visibility = "yes";
        sub-ass-line-spacing = 1;
        sub-ass-hinting = "normal";
        sub-ass-override = "force";
        save-position-on-quit = true;
        sub-auto-exts = "srt,ass,txt";
        ytdl-format = "bestvideo+bestaudio/best";
        slang = "fin,fi,fi-fi,eng,en,en-en,en-orig";
        sub-font = "${config.stylix.fonts.serif.name}";
        sub-ass-force-style = "${config.stylix.fonts.serif.name}";
        sub-color = "${config.lib.stylix.colors.withHashtag.base07}";
        sub-shadow-color = "${config.lib.stylix.colors.withHashtag.base00}";
        watch-later-options-clr = true; # Dont save settings like brightness
      };
      scripts = [
        pkgs.mpvScripts.uosc
        pkgs.mpvScripts.acompressor
        pkgs.mpvScripts.autoload
      ];
    };
  };
}
