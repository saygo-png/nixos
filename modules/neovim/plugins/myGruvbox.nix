_: {
  programs.nixvim = {
    highlightOverride = {
      WinBar.bg = "NONE";
      WinBarNC.bg = "NONE"; # not focused

      "@constructor".link = "";
      "@punctuation.special".link = "";
      "@punctuation.delimiter".link = "";
      "DiagnosticDeprecated".link = "GruvboxAquaUnderline";
    };

    colorschemes.gruvbox = {
      enable = true;
      lazyLoad.enable = true;
      settings = {
        bold = false;
        undercurl = true;
        underline = true;
        strikethrough = false;
        transparent_mode = true;
      };
    };
  };
}
