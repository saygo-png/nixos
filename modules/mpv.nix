{
  lib,
  pkgs,
  conUsername,
  ...
}: {
  home-manager.users.${conUsername} = {config, ...}: {
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
        save-watch-history = true;
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
        pkgs.mpvScripts.autoload
        pkgs.mpvScripts.videoclip
        # pkgs.mpvScripts.acompressor
      ];
    };
  };
}
