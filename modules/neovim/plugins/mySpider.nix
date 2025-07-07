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

    # Needed for special character handling
    extraLuaPackages = luaPkgs: [luaPkgs.luautf8];
  };
}
