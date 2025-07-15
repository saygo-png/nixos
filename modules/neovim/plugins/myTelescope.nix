{
  lib,
  config,
  inputs,
  pkgs,
  ...
}: {
  programs.nixvim = {
    plugins = {
      telescope = {
        enable = true;
        lazyLoad.settings.cmd = "Telescope";
        enabledExtensions = ["git_file_history"];
        extensions.fzf-native = {
          enable = true;
          settings = {
            fuzzy = true;
            override_file_sorter = true;
            override_generic_sorter = true;
          };
        };

        settings = {
          defaults = {
            mappings = {
              n = {
                "<C-f>" = {
                  __raw = "require('telescope.actions.layout').toggle_preview";
                };
              };
              i = {
                "<M-f>" = {
                  __raw = "require('telescope.actions.layout').toggle_preview";
                };
              };
            };
          };
        };

        keymaps = {
          "<leader>tb" = {
            action = "current_buffer_fuzzy_find";
            options.desc = "[t]elescope [b]uffer";
          };
          "<leader>tn" = {
            action = "help_tags";
            options.desc = "[t]elescope [n]oob";
          };
          "<leader>tk" = {
            action = "keymaps";
            options.desc = "[t]elescope [k]eymaps";
          };
          "<leader>tf" = {
            action = "find_files";
            options.desc = "[t]elescope [f]iles";
          };
          "<leader>ts" = {
            action = "builtin";
            options.desc = "[t]elescope [s]elect telescope";
          };
          "<leader>tw" = {
            action = "grep_string";
            options.desc = "[t]elescope current [w]ord";
          };
          "<leader>tl" = {
            action = "live_grep";
            options.desc = "[t]elescope [l]ive grep";
          };
          "<leader>td" = {
            action = "diagnostics";
            options.desc = "[t]elescope [d]iagnostics";
          };
          "<leader>tr" = {
            action = "resume";
            options.desc = "[t]elescope [r]esume";
          };
          "<leader>t." = {
            action = "oldfiles";
            options.desc = "[t]elescope recent files (. for repeat)";
          };
          "<leader><leader>" = {
            action = "buffers";
            options.desc = "Find existing buffers";
          };
        };
      };

      which-key.settings.spec = [(lib.my.nRegister "<leader>t" "Telescope" " ")];
      project-nvim.enableTelescope = true;
      harpoon.enableTelescope = true;
    };

    keymaps =
      [
        {
          key = "<leader>to";
          action.__raw = "function()
            require'telescope.builtin'.live_grep({ grep_open_files = true, prompt_title = 'Live Grep in Open Files' })
          end";
          options.desc = "[S]earch in [O]pen Files";
        }
        {
          key = "<leader>tcf";
          action.__raw = "function()
            require'telescope.builtin'.find_files({ cwd = require'telescope.utils'.buffer_dir() })
          end";
          options.desc = "[t]elescope find [f]iles in [c]urrent dir";
        }
        {
          key = "<leader>tcg";
          action.__raw = "function()
            require'telescope.builtin'.live_grep({ cwd = require'telescope.utils'.buffer_dir() })
          end";
          options.desc = "[t]elescope grep in [c]urrent dir";
        }
        {
          key = "<leader>tg";
          action.__raw = "function()
            require'telescope.builtin'.grep_string({ shorten_path = true, word_match = '-w', only_sort_text = true, search = '' })
          end";
          options.desc = "[t]elescope fuzzy [g]rep";
        }
        {
          key = "<leader>tv";
          action.__raw = "function()
            require'telescope'.extensions.git_file_history.git_file_history()
          end";
          options.desc = "[t]elescope [v]ersions";
        }
      ]
      ++ lib.optionals config.programs.nixvim.plugins.harpoon.enable [
        {
          action = "<cmd>Telescope harpoon marks<CR>";
          key = "<Leader>th";
          options.desc = "[t]elescope [h]arpoon Marks";
        }
      ];

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
  };
}
