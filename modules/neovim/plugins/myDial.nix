_: {
  programs.nixvim = {
    plugins.dial = {
      enable = true;
      lazyLoad.settings.event = "DeferredUIEnter";
      luaConfig.post = ''
        local augend = require("dial.augend")
        require("dial.config").augends:register_group{
          default = {
            augend.constant.alias.alpha,
            augend.constant.alias.Alpha,
            augend.constant.alias.bool,
            augend.date.alias["%-d.%-m."],
            augend.date.alias["%d.%m."],
            augend.date.alias["%d/%m/%y"],
            augend.date.alias["%d/%m/%Y"],
            augend.date.alias["%H:%M"],
            augend.date.alias["%H:%M:%S"],
            augend.date.alias["%-m/%-d"],
            augend.date.alias["%m/%d"],
            augend.date.alias["%m/%d/%y"],
            augend.date.alias["%m/%d/%Y"],
            augend.date.alias["%Y/%m/%d"],
            augend.integer.alias.binary,
            augend.integer.alias.decimal,
            augend.integer.alias.decimal_int,
            augend.integer.alias.hex,
            augend.integer.alias.octal,
            augend.semver.alias.semver,
          },
          typescript = {
            augend.constant.new{ elements = {"let", "const"} },
          },
        }
        vim.keymap.set("n", "<C-a>",  function() require("dial.map").manipulate("increment", "normal")  end)
        vim.keymap.set("n", "<C-x>",  function() require("dial.map").manipulate("decrement", "normal")  end)
        vim.keymap.set("n", "g<C-a>", function() require("dial.map").manipulate("increment", "gnormal") end)
        vim.keymap.set("n", "g<C-x>", function() require("dial.map").manipulate("decrement", "gnormal") end)
        vim.keymap.set("v", "<C-a>",  function() require("dial.map").manipulate("increment", "visual")  end)
        vim.keymap.set("v", "<C-x>",  function() require("dial.map").manipulate("decrement", "visual")  end)
        vim.keymap.set("v", "g<C-a>", function() require("dial.map").manipulate("increment", "gvisual") end)
        vim.keymap.set("v", "g<C-x>", function() require("dial.map").manipulate("decrement", "gvisual") end)
      '';
    };
  };
}
