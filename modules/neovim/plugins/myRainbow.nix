{
  inputs,
  pkgs,
  ...
}: {
  programs.nixvim = {
    extraPlugins = [
      (pkgs.vimUtils.buildVimPlugin {
        name = "rainbow";
        src = inputs.nvim-plugin-rainbow;
      })
    ];

    globals.rainbow_active = 1;

    extraConfigLuaPost = ''
      -- Makes treesitter work with rainbow plugin
      vim.api.nvim_set_hl(0, "@constructor", { link = "" })
      vim.api.nvim_set_hl(0, "@punctuation.bracket", { link = "" })
      vim.api.nvim_set_hl(0, "@punctuation.special", { link = "" })
      vim.api.nvim_set_hl(0, "@punctuation.delimiter", { link = "" })
      vim.api.nvim_set_hl(0, "@variable.parameter.haskell", { link = "" })
    '';
  };
}
