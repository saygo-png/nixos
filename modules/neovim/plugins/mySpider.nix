_: {
  programs.nixvim = {
    plugins.spider = {
      enable = true;
      lazyLoad.settings.keys = ["b" "e" "ge" "w"];
      settings = {
        consistentOperatorPending = true;
        skipInsignificantPunctuation = false;
      };
      keymaps.motions = {
        b = "b";
        e = "e";
        ge = "ge";
        w = "w";
      };
    };
  };
}
