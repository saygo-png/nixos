{lib, ...}: {
  programs.nixvim = {
    plugins.flash = {
      enable = true;
      settings.autojump = true;
      lazyLoad.settings.keys = ["s" "S" "gs"];
    };

    keymaps = let
      inherit (lib.my) nWrapFunc;
    in [
      {
        key = "s";
        action.__raw = nWrapFunc ''require("flash").remote()'';
        options.desc = "Flash";
      }
      {
        key = "S";
        action.__raw = nWrapFunc ''require("flash").treesitter_search()'';
        options.desc = "Flash treesitter";
      }
      {
        key = "gs";
        action.__raw = nWrapFunc ''require("flash").treesitter()'';
        options.desc = "Flash treesitter";
      }
    ];
  };
}
