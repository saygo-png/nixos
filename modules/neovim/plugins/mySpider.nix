_: {
  programs.nixvim = {
    plugins.spider = {
      enable = true;
      skipInsignificantPunctuation = true;
      extraOptions.consistentOperatorPending = true;
      keymaps.motions = {
        b = "b";
        e = "e";
        ge = "ge";
        w = "w";
      };
    };
  };
}
