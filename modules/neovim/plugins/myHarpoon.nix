{lib, ...}: {
  programs.nixvim = {
    plugins = {
      harpoon = {
        enable = true;
        luaConfig.post = ''
          local harpoon = require("harpoon")
          local harpoon_extensions = require("harpoon.extensions")
          harpoon:extend(harpoon_extensions.builtins.highlight_current_file())
        '';
      };

      which-key.settings.spec = [
        (lib.my.nRegister "<leader>h" "Harpoon" "󱢓 ")
      ];
    };

    keymaps = let
      mkBind = key: a: d: {
        mode = "n";
        inherit key;
        options.desc = d;
        action.__raw = "function() require'harpoon'${a} end";
      };
    in [
      (mkBind "<leader>ha" ":list():add()" "Add file to harpoon list")
      (mkBind "<leader>hm" ".ui:toggle_quick_menu(require'harpoon':list())" "Harpoon menu")
      (mkBind "<leader>hn" ":list():next()" "Harpoon next")
      (mkBind "<leader>hp" ":list():prev()" "Harpoon previous")
      (mkBind "<C-h>" ":list():select(1)" "Harpoon 1")
      (mkBind "<C-j>" ":list():select(2)" "Harpoon 2")
      (mkBind "<C-k>" ":list():select(3)" "Harpoon 3")
      (mkBind "<C-l>" ":list():select(4)" "Harpoon 4")
    ];
  };
}
