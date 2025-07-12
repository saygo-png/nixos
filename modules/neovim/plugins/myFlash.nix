_: {
  programs.nixvim = {
    plugins.flash = {
      enable = true;
      settings.autojump = true;
    };

    keymaps = [
      {
        key = "s";
        action.__raw = ''require("flash").remote'';
        options.desc = "Flash";
      }
      {
        key = "S";
        action.__raw = ''require("flash").treesitter_search'';
        options.desc = "Flash treesitter";
      }
      {
        key = "gs";
        action.__raw = ''require("flash").treesitter'';
        options.desc = "Flash treesitter";
      }
    ];
  };
}
