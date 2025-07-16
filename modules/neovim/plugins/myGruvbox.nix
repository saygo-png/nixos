_: {
  programs.nixvim = {
    highlightOverride = {
      WinBar.bg = "NONE";
      WinBarNC.bg = "NONE"; # not focused

      "@constructor".link = "";
      "@punctuation.special".link = "";
      "@punctuation.delimiter".link = "";
    };

    colorschemes.gruvbox = {
      enable = true;
      lazyLoad.enable = true;
      settings = {
        undercurl = true;
        bold = false;
        underline = true;
        transparent_mode = true;
      };
    };
  };
}
