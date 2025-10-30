{conUsername, ...}: {
  home-manager.users.${conUsername} = {
    config,
    osConfig,
    ...
  }: {
    xdg = {
      configFile."mimeapps.list".force = true;
      mimeApps = {
        enable = true;
        associations.added = config.xdg.mimeApps.defaultApplications;
        defaultApplications = let
          videoPlayer = "mpv.desktop";
          imageViewer = "nsxiv.desktop";
          terminal = "Alacritty.desktop";
          pdfViewer = "org.pwmt.zathura.desktop";
          fileBrowser = "org.kde.dolphin.desktop";
          inherit (osConfig.environment.variables) EDITOR;
          inherit (osConfig.environment.variables) BROWSER;
        in {
          "text/plain" = "${EDITOR}.desktop";
          "text/rhtml" = "${EDITOR}.desktop";
          "text/x-tex" = "${EDITOR}.desktop";
          "text/x-java" = "${EDITOR}.desktop";
          "text/x-ruby" = "${EDITOR}.desktop";
          "text/x-cmake" = "${EDITOR}.desktop";
          "text/markdown" = "${EDITOR}.desktop";
          "text/x-python" = "${EDITOR}.desktop";
          "text/x-readme" = "${EDITOR}.desktop";
          "text/x-markdown" = "${EDITOR}.desktop";
          "application/json" = "${EDITOR}.desktop";
          "application/x-ruby" = "${EDITOR}.desktop";
          "application/x-yaml" = "${EDITOR}.desktop";
          "application/x-docbook+xml" = "${EDITOR}.desktop";
          "application/x-shellscript" = "${EDITOR}.desktop";

          "image/bmp" = imageViewer;
          "image/gif" = imageViewer;
          "image/jpg" = imageViewer;
          "image/jxl" = imageViewer;
          "image/png" = imageViewer;
          "image/avif" = imageViewer;
          "image/heif" = imageViewer;
          "image/jpeg" = imageViewer;
          "image/tiff" = imageViewer;
          "image/webp" = imageViewer;
          "image/x-eps" = imageViewer;
          "image/x-ico" = imageViewer;
          "image/x-psd" = imageViewer;
          "image/x-tga" = imageViewer;
          "image/x-icns" = imageViewer;
          "image/x-webp" = imageViewer;
          "image/svg+xml" = imageViewer;
          "image/x-xbitmap" = imageViewer;
          "image/x-xpixmap" = imageViewer;
          "image/x-portable-bitmap" = imageViewer;
          "image/x-portable-pixmap" = imageViewer;
          "image/x-portable-graymap" = imageViewer;

          "image/vnd.djvu" = pdfViewer;
          "application/pdf" = pdfViewer;

          "video/dv" = videoPlayer;
          "video/3gp" = videoPlayer;
          "video/avi" = videoPlayer;
          "video/fli" = videoPlayer;
          "video/flv" = videoPlayer;
          "video/mp4" = videoPlayer;
          "video/ogg" = videoPlayer;
          "video/3gpp" = videoPlayer;
          "video/divx" = videoPlayer;
          "video/mp2t" = videoPlayer;
          "video/mpeg" = videoPlayer;
          "video/webm" = videoPlayer;
          "video/3gpp2" = videoPlayer;
          "video/x-avi" = videoPlayer;
          "video/x-flv" = videoPlayer;
          "video/x-m4v" = videoPlayer;
          "video/x-ogm" = videoPlayer;
          "video/mp4v-es" = videoPlayer;
          "video/msvideo" = videoPlayer;
          "video/x-mpeg2" = videoPlayer;
          "video/vnd.divx" = videoPlayer;
          "video/x-ms-asf" = videoPlayer;
          "video/x-ms-wmv" = videoPlayer;
          "video/x-ms-wmx" = videoPlayer;
          "video/x-theora" = videoPlayer;
          "video/quicktime" = videoPlayer;
          "video/x-msvideo" = videoPlayer;
          "video/x-ogm+ogg" = videoPlayer;
          "video/x-matroska" = videoPlayer;
          "video/vnd.mpegurl" = videoPlayer;
          "video/x-theora+ogg" = videoPlayer;
          "application/x-matroska" = videoPlayer;
          "video/vnd.rn-realvideo" = videoPlayer;

          "audio/aac" = videoPlayer;
          "audio/mp4" = videoPlayer;
          "audio/ogg" = videoPlayer;
          "audio/mpeg" = videoPlayer;
          "audio/x-mp3" = videoPlayer;
          "audio/x-wav" = videoPlayer;
          "audio/vorbis" = videoPlayer;
          "audio/x-flac" = videoPlayer;
          "audio/mpegurl" = videoPlayer;
          "audio/x-scpls" = videoPlayer;
          "audio/x-speex" = videoPlayer;
          "audio/x-ms-wma" = videoPlayer;
          "audio/x-vorbis" = videoPlayer;
          "audio/x-mpegurl" = videoPlayer;
          "audio/x-oggflac" = videoPlayer;
          "audio/x-musepack" = videoPlayer;
          "audio/x-vorbis+ogg" = videoPlayer;
          "audio/x-pn-realaudio" = videoPlayer;
          "audio/vnd.rn-realaudio" = videoPlayer;

          "inode/directory" = fileBrowser;
          "terminal" = terminal;
          "text/html" = "${BROWSER}.desktop";
          "x-scheme-handler/ftp" = "${BROWSER}.desktop";
          "application/xhtml+xml" = "${BROWSER}.desktop";
          "x-scheme-handler/http" = "${BROWSER}.desktop";
          "x-scheme-handler/https" = "${BROWSER}.desktop";
          "x-scheme-handler/chrome" = "${BROWSER}.desktop";
          "application/x-extension-htm" = "${BROWSER}.desktop";
          "application/x-extension-xht" = "${BROWSER}.desktop";
          "application/x-extension-html" = "${BROWSER}.desktop";
          "application/x-extension-shtml" = "${BROWSER}.desktop";
          "application/x-extension-xhtml" = "${BROWSER}.desktop";
        };
      };
    };
  };
}
