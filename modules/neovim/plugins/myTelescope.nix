{
  lib,
  inputs,
  pkgs,
  ...
}: {
  programs.nixvim = {
    plugins = {
      telescope = {
        enable = true;
        enabledExtensions = ["git_file_history"];
        extensions.fzf-native = {
          enable = true;
          settings = {
            fuzzy = true;
            override_file_sorter = true;
            override_generic_sorter = true;
          };
        };
      };

      which-key.settings.spec = [(lib.my.nRegister "<leader>t" "Telescope" " ")];
      project-nvim.enableTelescope = true;
      harpoon.enableTelescope = true;
    };

    highlightOverride = {
      TelescopeBorder.link = "LineNr";
      TelescopePromptBorder.link = "LineNr";
      TelescopeResultsBorder.link = "LineNr";
      TelescopePreviewBorder.link = "LineNr";
    };

    extraPlugins = [
      (pkgs.vimUtils.buildVimPlugin {
        name = "telescope-git-file-history";
        src = inputs.nvim-plugin-telescope-git-file-history;
      })
    ];

    extraConfigLua = ''
      local utils = require "telescope.utils"
      local builtin = require "telescope.builtin"

      vim.keymap.set("n", "<leader>th", "<cmd>Telescope harpoon marks<CR>", { silent = true, desc = "[t]elescope [h]arpoon Marks" })
      vim.keymap.set("n", "<leader>tcf", function()
        builtin.find_files({ cwd = utils.buffer_dir() })
      end, { silent = true, desc = "[t]elescope find [f]iles in [c]urrent buffer" })
      vim.keymap.set("n", "<leader>tcg", function()
        builtin.live_grep({ cwd = utils.buffer_dir() })
      end, { silent = true, desc = "[t]elescope grep in [c]urrent buffer" })

      vim.keymap.set("n", "<leader>tb", builtin.current_buffer_fuzzy_find, { desc = "[t]elescope [b]uffer" })
      vim.keymap.set("n", "<leader>tn", builtin.help_tags, { desc = "[t]elescope [n]oob" })
      vim.keymap.set("n", "<leader>tk", builtin.keymaps, { desc = "[t]elescope [k]eymaps" })
      vim.keymap.set("n", "<leader>tf", builtin.find_files, { desc = "[t]elescope [f]iles" })
      vim.keymap.set("n", "<leader>ts", builtin.builtin, { desc = "[t]elescope [s]elect telescope" })
      vim.keymap.set("n", "<leader>tw", builtin.grep_string, { desc = "[t]elescope current [w]ord" })
      vim.keymap.set("n", "<leader>tl", builtin.live_grep, { desc = "[t]elescope [l]ive grep" })

      local fuzzy_search = function()
        builtin.grep_string({ shorten_path = true, word_match = "-w", only_sort_text = true, search = "" })
      end
      vim.keymap.set("n", "<leader>tg", fuzzy_search, { desc = "[t]elescope fuzzy [g]rep" })

      vim.keymap.set("n", "<leader>td", builtin.diagnostics, { desc = "[t]elescope [d]iagnostics" })
      vim.keymap.set("n", "<leader>tr", builtin.resume, { desc = "[t]elescope [r]esume" })
      vim.keymap.set("n", "<leader>t.", builtin.oldfiles, { desc = "[t]elescope recent files (. for repeat)" })
      vim.keymap.set("n", "<leader><leader>", builtin.buffers, { desc = "Find existing buffers" })

      -- Telescope extensions
      local gfh = require("telescope").extensions.git_file_history
      vim.keymap.set("n", "<leader>tv", gfh.git_file_history, { desc = "[t]elescope [v]ersions" })
    '';
  };
}
