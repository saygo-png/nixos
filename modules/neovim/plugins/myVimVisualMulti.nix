_: {
  programs.nixvim = {
    plugins.visual-multi = {
      enable = true;
      settings = {
        leader = ''\'';
        silent_exit = 1;
        skip_empty_lines = 1;
        maps = {
          "Find Subword Under" = "<C-d>";
          "Add Cursor Down" = "<M-j>";
          "Add Cursor Up" = "<M-k>";
        };
      };
    };
  };
}
