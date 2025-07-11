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
      settings = {
        undercurl = false;
        underline = false;
        transparent_mode = true;
      };
    };
  };
}
