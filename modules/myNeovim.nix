{
  lib,
  pkgs,
  config,
  conUsername,
  conAccentColor,
  conFlakePathRel,
  conRefresh-rate,
  ...
}: {
  environment.systemPackages = with pkgs; [
    neovide # Neovim gui
  ];

  home-manager.users.${conUsername} = {
    inputs,
    ...
  }: {
    home.shellAliases = {"neov" = "neovide";};
    home.sessionVariables = {EDITOR = "nvim";};
    programs.nixvim = {
      enable = true;
      extraPackages = with pkgs; [
        typos-lsp
        vale
        jq # Json formatter
        vim-language-server
        deadnix # Nix linter
        pyright # python lsp
        nodePackages.jsonlint
        hlint # Haskell linter
        stylua # Lua formatter
        marksman # Markdown LSP
        shfmt # Shell formatter
        yapf # Python formatter
        black # Python formatter
        hadolint # Docker linter
        rust-analyzer # Rust LSP
        shellcheck # Bash linter
        clojure-lsp # Clojure lsp
        cljfmt # Clojure formatter
        clj-kondo # Clojure linter
        zprint # Clojure formatter
        sumneko-lua-language-server
        isort # Python import sorter
        prettierd # Javascript formatter
        nodePackages.bash-language-server
        markdownlint-cli # Markdown linter
        stylish-haskell # Haskell formatter
        haskell-language-server # Haskell lsp
        vscode-langservers-extracted # Web LSPs
        python312Packages.mccabe # Flake8 plugin
        python312Packages.pyflakes # Python linter
        haskellPackages.fourmolu # Haskell formatter
        luajitPackages.jsregexp # Needed for luasnip
        nodePackages.prettier # Javascript formatter
        python312Packages.jedi # Autocomplete plugin
        python312Packages.pyls-isort # Python import sort
      ];

      highlightOverride = {
        # hi noCursor blend=100 cterm=strikethrough
        noCursor.blend = 100;
        statusline.bg = "NONE";
        ModeMsg.fg = "#${conAccentColor}";
        MsgArea.fg = "#${conAccentColor}";
        statusline.fg = "#${conAccentColor}";
        FloatBorder.fg = "#${conAccentColor}";
        CursorLineNr.fg = "#${conAccentColor}";
        CursorLineNr.bg = "${config.lib.stylix.colors.withHashtag.base01}"; # Gray numberline
        MiniIndentscopeSymbol.fg = "${config.lib.stylix.colors.withHashtag.base01}"; # Gray indentline
      };

      opts = {
        # Indents.
        tabstop = 2;
        shiftwidth = 2;
        softtabstop = 2;
        expandtab = true;
        autoindent = true;
        breakindent = true; # Indent when wrapping

        # Wrapping.
        linebreak = true;
        wrap = false;

        # Center it all.
        scrolloff = 999;
        sidescrolloff = 999;

        # Delay on switching to normal mode.
        ttimeoutlen = 0;

        # g in substitute implicit
        gdefault = true;

        # Incremental search.
        incsearch = true;
        updatetime = 100;

        # Relative numberline on the left.
        number = true;
        relativenumber = true;

        # Color current line number.
        cursorline = true;
        cursorlineopt = "number";

        # Smartcase search and ripgrep.
        smartcase = true;
        ignorecase = true;
        grepprg = "rg --vimgrep";
        grepformat = "%f:%l:%c:%m";

        # Folds.
        foldenable = false;
        foldmethod = "marker";

        # More space.
        cmdheight = 0;

        # Puts error messages on the number line.
        signcolumn = "number";

        # Show some whitespace.
        list = true;
        listchars = "tab:▸ ,trail:·,nbsp:␣";

        # Better completion.
        completeopt = ["menuone" "noselect" "noinsert"];

        # Use conform-nvim for gq formatting. ('formatexpr' is set to vim.lsp.formatexpr(),
        # so you can format lines via gq if the language server supports it).
        formatexpr = "v:lua.require'conform'.formatexpr()";

        # (https://neovim.io/doc/user/options.html#'laststatus')
        laststatus = 3;
      };
      globals = {
        mapleader = " ";
        maplocalleader = ",";

        rainbow_active = 1;

        gruvbox_material_foreground = "original";
        gruvbox_material_enable_bold = 0;
        gruvbox_material_transparent_background = 2;

        # Neovide neovim gui client.
        neovide_transparency = config.stylix.opacity.terminal;
        neovide_transparency_point = 0;
        neovide_background_color = "${config.lib.stylix.colors.withHashtag.base00}";
        neovide_padding_top = lib.mkDefault 8;
        neovide_padding_bottom = lib.mkDefault 0;
        neovide_padding_right = lib.mkDefault 6;
        neovide_padding_left = lib.mkDefault 6;
        neovide_floating_blur_amount_x = 20.0;
        neovide_floating_blur_amount_y = 20.0;
        neovide_hide_mouse_when_typing = true;
        neovide_refresh_rate = conRefresh-rate;
        neovide_cursor_vfx_mode = "ripple";
        neovide_cursor_animation_length = 0.08;
        neovide_cursor_smooth_blink = true;
        neovide_floating_shadow = false;
        neovide_cursor_animate_command_line = true;
        neovide_cursor_animate_in_insert_mode = true;
      };

      extraFiles = {
        "ftplugin/json.vim" = ''
          setlocal foldmethod=manual
        '';
        "ftplugin/markdown.vim" = ''
          setlocal wrap
        '';
      };

      extraConfigVim = builtins.readFile "${conFlakePathRel}/resources/nvim-extraConfig.vim";
      extraConfigLuaPre = ''
        -- Hide deprecation warnings, i used this as a fix to
        -- multicursors plugin, but might be useful later on
        -- local notify = vim.notify
        -- vim.notify = function(msg, ...)
        --   if msg:match("has been deprecated") then
        --     return
        --   end
        --   notify(msg, ...)
        -- end
      '';
      extraConfigLuaPost = ''
        -- Makes treesitter work with rainbow plugin
        vim.api.nvim_set_hl(0, "@constructor", { link = "" })
        -- vim.api.nvim_set_hl(0, "@constructor.lua", { link = "" })
        vim.api.nvim_set_hl(0, "@punctuation.bracket", { link = "" })
        vim.api.nvim_set_hl(0, "@punctuation.special", { link = "" })
        vim.api.nvim_set_hl(0, "@punctuation.delimiter", { link = "" })
        vim.api.nvim_set_hl(0, "@variable.parameter.haskell", { link = "" })
      '';

      extraConfigLua = ''
        -- Vim as terminal
        vim.cmd[[
          augroup neovim_terminal
              autocmd!
              " Enter Terminal-mode (insert) automatically
              autocmd TermOpen * startinsert
              " Disables number lines on terminal buffers
              autocmd TermOpen * :setlocal nonumber norelativenumber laststatus=0
          augroup END

          " Vim visual multi binds
          let g:VM_leader = '\'
          let g:VM_maps = {}
          let g:VM_maps["Add Cursor Down"] = '<M-j>'
          let g:VM_maps["Add Cursor Up"] = '<M-k>'
        ]]

        -- Neovide
        if vim.g.neovide then
          vim.cmd[[colorscheme gruvbox-material]]
          vim.o.background = 'dark'
          vim.o.guifont = '${config.stylix.fonts.monospace.name}:h${builtins.toString (config.stylix.fonts.sizes.terminal + 1)}:#e-antialias:#h-slight'
          vim.cmd [[ hi Normal guibg=${config.lib.stylix.colors.withHashtag.base00} ]]
        end

        local open_command = "xdg-open"
        if vim.fn.has("mac") == 1 then
          open_command = 'open'
        end

        local function url_repo()
          local cursorword = vim.fn.expand('<cfile>')
          if string.find(cursorword, '^[a-zA-Z0-9-_.]*/[a-zA-Z0-9-_.]*$') then
            cursorword = "https://github.com/" .. cursorword
          end
          return cursorword or ""
        end

        vim.keymap.set('n', 'gx', function()
          vim.fn.jobstart({ open_command, url_repo() }, { detach = true })
        end, { silent = true })

        -- Keymaps
        -- Keep selection when indenting.
        vim.keymap.set("v", ">", ">gv", { desc = "Keep selection after indenting" })
        vim.keymap.set("v", "<", "<gv", { desc = "Keep selection after unindenting" })

        -- Keep cursor position after yank
        vim.keymap.set("n", "y", "ygv<esc>", { desc = "Keep cursor position after yank" })

        -- Window switching.
        vim.keymap.set("n", "<C-h>", ":wincmd h<CR>", { desc = "Move to the split on the left side" })
        vim.keymap.set("n", "<C-l>", ":wincmd l<CR>", { desc = "Move to the split on the right side" })
        vim.keymap.set("n", "<C-k>", ":wincmd k<CR>", { desc = "Move to the split above" })
        vim.keymap.set("n", "<C-j>", ":wincmd j<CR>", { desc = "Move to the split below" })

        -- Previous buffer
        vim.keymap.set('n', '<S-B>', '<C-6>')

        -- Conflicts with lsp hover
        vim.g["conjure#mapping#doc_word"] = false

        -- Split movement
        vim.keymap.set("n", "<S-M-h>", "<cmd>wincmd h<CR>", { desc = "Move to the split on the left side" })
        vim.keymap.set("n", "<S-M-l>", "<cmd>wincmd l<CR>", { desc = "Move to the split on the right side" })
        vim.keymap.set("n", "<S-M-k>", "<cmd>wincmd k<CR>", { desc = "Move to the split above" })
        vim.keymap.set("n", "<S-M-j>", "<cmd>wincmd j<CR>", { desc = "Move to the split below" })
        -- In nvim terminal
        vim.keymap.set("t", "<S-M-h>", "<c-\\><c-n><c-w>h", { desc = "Move to the split on the left side" })
        vim.keymap.set("t", "<S-M-l>", "<c-\\><c-n><c-w>j", { desc = "Move to the split on the right side" })
        vim.keymap.set("t", "<S-M-k>", "<c-\\><c-n><c-w>k", { desc = "Move to the split above" })
        vim.keymap.set("t", "<S-M-j>", "<c-\\><c-n><c-w>l", { desc = "Move to the split below" })
        -- Shift + Esc for normal mode in nvim terminal
        vim.keymap.set("t", "<S-M-Esc>", "<C-\\><C-n>", { desc = "Normal mode in terminal mode" })
        vim.keymap.set("t", "<S-M-Esc>", "<C-\\><C-n>", { desc = "Normal mode in terminal mode" })

        -- Plugins
        local utils = require "telescope.utils"
        local builtin = require "telescope.builtin"

        vim.keymap.set("n", "<leader>f", "<cmd>Oil<CR>", {desc = "[f]ile browser"})
        vim.keymap.set("n", "K", "<cmd>Lspsaga hover_doc<CR>", {desc = "hover"})
        vim.keymap.set("n", "<leader>a", "<cmd>Lspsaga code_action<CR>", {desc = "code [a]ctions"})
        vim.keymap.set("n", "<leader>th", "<cmd>Telescope harpoon marks<CR>", { silent = true, desc = "[t]elescope [h]arpoon Marks" })
        vim.keymap.set("n", "<leader>tb", builtin.current_buffer_fuzzy_find, { desc = "[t]elescope [b]uffer" })
        vim.keymap.set("n", "<leader>tn", builtin.help_tags, { desc = "[t]elescope [n]oob" })
        vim.keymap.set("n", "<leader>tk", builtin.keymaps, { desc = "[t]elescope [k]eymaps" })
        vim.keymap.set("n", "<leader>tf", builtin.find_files, { desc = "[t]elescope [f]iles" })
        vim.keymap.set("n", "<leader>ts", builtin.builtin, { desc = "[t]elescope [s]elect telescope" })
        vim.keymap.set("n", "<leader>tw", builtin.grep_string, { desc = "[t]elescope current [w]ord" })
        vim.keymap.set("n", "<leader>tg", builtin.live_grep, { desc = "[t]elescope by [g]rep" })
        vim.keymap.set("n", "<leader>td", builtin.diagnostics, { desc = "[t]elescope [d]iagnostics" })
        vim.keymap.set("n", "<leader>tr", builtin.resume, { desc = "[t]elescope [r]esume" })
        vim.keymap.set("n", "<leader>t.", builtin.oldfiles, { desc = "[t]elescope recent files (. for repeat)" })
        vim.keymap.set("n", "<leader><leader>", builtin.buffers, { desc = "[ ] Find existing buffers" })
        -- dial.nvim
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

        vim.keymap.set("n", "<leader>rn", function()
          -- when rename opens the prompt, this autocommand will trigger
          -- it will "press" CTRL-F to enter the command-line window `:h cmdwin`
          -- in this window I can use normal mode keybindings
          local cmdId
          cmdId = vim.api.nvim_create_autocmd({ "CmdlineEnter" }, {
            callback = function()
              local key = vim.api.nvim_replace_termcodes("<C-f>", true, false, true)
              vim.api.nvim_feedkeys(key, "c", false)
              vim.api.nvim_feedkeys("0", "n", false)
              -- autocmd was triggered and so we can remove the ID and return true to delete the autocmd
              cmdId = nil
              return true
            end,
          })
          vim.lsp.buf.rename()
          -- if LPS couldn't trigger rename on the symbol, clear the autocmd
          vim.defer_fn(function()
            -- the cmdId is not nil only if the LSP failed to rename
            if cmdId then
              vim.api.nvim_del_autocmd(cmdId)
            end
          end, 500)
        end)

        vim.keymap.set("n", "<Leader>c", function()
        require("conform").format({ timeout_ms = 500 })
        end, { desc = "[c]onform" })

        vim.keymap.set("n", "<leader>tcf", function()
          builtin.find_files({ cwd = utils.buffer_dir() })
        end, { silent = true, desc = "[t]elescope find [f]iles in [c]urrent buffer" })

        vim.keymap.set("n", "<leader>tcg", function()
          builtin.live_grep({ cwd = utils.buffer_dir() })
        end, { silent = true, desc = "[t]elescope grep in [c]urrent buffer" })

        -- Clipboard
        vim.keymap.set("n", "<c-v>", '"+p', { desc = "proper paste" })
        vim.keymap.set({"i", "c"}, "<C-V>", "<C-r>+", { desc = "Proper paste" })

        -- Basic
        vim.keymap.set("n", ";", ":", { desc = "Command mode with or without shift"})
        vim.keymap.set("n", ";", ":", { desc = "Command mode with or without shift"})
        vim.keymap.set("n", ";", ":", { desc = "Command mode with or without shift"})
        vim.keymap.set("n", ">", ">>", { desc = "Indent more", silent = true })
        vim.keymap.set("n", "<lt>", "<lt><lt>", { desc = "Indent less", silent = true })
        vim.keymap.set("v", ".", "<cmd>normal .<CR>", { desc = "Dot commands over visual blocks" })
        vim.keymap.set("n", "G", "Gzz", { desc = "Center bottom" })
        vim.keymap.set("n", "gg", "ggzz", { desc = "Center top" })
        vim.keymap.set("n", "gm", "m", { desc = "Set mark" })
        vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")
        vim.keymap.set("v", "gj", "J", { desc = "join lines" })
        vim.keymap.set("v", "J", ":m '>+1<CR>gv==kgvo<esc>=kgvo", { desc = "move highlighted text down" })
        vim.keymap.set("v", "K", ":m '<-2<CR>gv==jgvo<esc>=jgvo", { desc = "move highlighted text up" })
        vim.keymap.set( "i", "<C-r>", "<C-r><C-o>", { desc = "Insert contents of named register. Inserts text literally, not as if you typed it." })

        -- Autocomplete
        vim.keymap.set("i", "<C-x>", "<C-x><C-o>", { desc = "Autocomplete" })

        vim.keymap.set('n', '<leader>q', vim.cmd.quit)
        vim.keymap.set('n', '<leader>Q', vim.cmd.only)

        -- Transparent hover
        vim.api.nvim_set_hl(0, 'NormalFloat', { link = 'Normal', })

        require("cutlass").setup({
          cut_key = "m",
          override_del = true,
          exclude = { "ns", "nS" }, -- Motion plugins rebind this
        })

        -- Make lsp popups pretty.
        local border = {
          { '┌', 'FloatBorder' },
          { '─', 'FloatBorder' },
          { '┐', 'FloatBorder' },
          { '│', 'FloatBorder' },
          { '┘', 'FloatBorder' },
          { '─', 'FloatBorder' },
          { '└', 'FloatBorder' },
          { '│', 'FloatBorder' },
        }

        local _border = "single"
        require('lspconfig.ui.windows').default_options = { border = _border }
        vim.lsp.handlers["textDocument/hover"] = vim.lsp.with( vim.lsp.handlers.hover, { border = _border })
        vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with( vim.lsp.handlers.signature_help, { border = _border })

        vim.diagnostic.config({
        underline = false,
        update_in_insert = false,
        virtual_text = false,
        signs = true,
          float = {
            win_options = {
              winblend = 100
            },
            border = border,
          }
        })
      '';
      package = pkgs.neovim-unwrapped;
      clipboard.register = "unnamedplus";

      colorschemes.base16.enable = lib.mkForce false;
      colorscheme = "gruvbox-material";

      extraPlugins = [
        (pkgs.vimUtils.buildVimPlugin {
          name = "cutlass.nvim";
          src = inputs.nvim-plugin-cutlass;
        })
        (pkgs.vimUtils.buildVimPlugin {
          name = "vim-visual-multi";
          src = inputs.nvim-plugin-vim-visual-multi;
        })
        (pkgs.vimUtils.buildVimPlugin {
          name = "rainbow";
          src = inputs.nvim-plugin-rainbow;
        })
        pkgs.vimPlugins.gruvbox-material
        pkgs.vimPlugins.dial-nvim
        pkgs.vimPlugins.vim-dispatch
        pkgs.vimPlugins.vim-jack-in
      ];

      keymaps = [
        {
          key = "s";
          action.__raw = ''require("flash").remote'';
          options.desc = "Flash";
        }
        {
          key = "S";
          action.__raw = ''require("flash").treesitter'';
          options.desc = "Flash treesitter";
        }
        {
          action = "<cmd>lua vim.diagnostic.open_float()<CR>";
          key = "<Leader>e";
          options.desc = "Open diagnostic";
        }
        {
          action = '':!awk '{ print length(), $0 | "sort -n | cut -d\\  -f2-" }'<CR><CR>'';
          key = "<Leader>s";
          options.silent = true;
          options.desc = "[s]ort lines by length";
        }
      ];

      # Plugins {{{
      plugins = {
        nix.enable = false;
        flash.enable = true;
        comment.enable = true;
        fugitive.enable = true;
        surround.enable = true;
        friendly-snippets.enable = true;

        # Lisps
        conjure.enable = false;
        parinfer-rust.enable = false;

        spider = {
          enable = true;
          skipInsignificantPunctuation = false;
          keymaps.motions = {
            b = "b";
            e = "e";
            ge = "ge";
            w = "w";
          };
        };

        harpoon = {
          enable = true;
          markBranch = true;
          enableTelescope = true;
          keymaps = {
            addFile = "<leader>ha";
            navFile = {
              "1" = "<C-h>";
              "2" = "<C-j>";
              "3" = "<C-k>";
              "4" = "<C-l>";
            };
            navNext = "<leader>hn";
            navPrev = "<leader>hp";
            toggleQuickMenu = "<leader>hm";
            cmdToggleQuickMenu = "<leader>hcm";
          };
        };

        nvim-colorizer = {
          enable = true;
          fileTypes = let
            css = {css = true;};
          in [
            "*"
            ({language = "css";} // css)
            ({language = "less";} // css)
            ({language = "sass";} // css)
            ({language = "scss";} // css)
            ({language = "stylus";} // css)
          ];
          bufTypes = [
            "*"
            "!prompt"
            "!popup"
          ];
          userDefaultOptions = {
            names = false;
          };
        };

        telescope = {
          enable = true;
          extensions.fzf-native.enable = true;
        };

        lspsaga = {
          enable = true;
          lightbulb = {
            enable = false;
            sign = false;
            virtualText = false;
          };
          symbolInWinbar.enable = false;
        };

        mini = {
          enable = true;
          modules = {
            indentscope = {
              draw = {
                delay = 0;
                priority = 2;
              };
              symbol = "│";
              options = {
                border = "top";
                indent_at_cursor = true;
                try_as_border = true;
              };
            };
            align = {};
          };
        };

        trouble = {
          enable = true;
          settings = {
            auto_close = true;
          };
        };

        treesitter = {
          enable = true;
          nixvimInjections = true;
          nixGrammars = true; # Install grammars with Nix
          ensureInstalled = ["all"];
          ignoreInstall = ["comment"];
          incrementalSelection = {
            enable = true;
            keymaps = {
              initSelection = "<Enter>";
              nodeIncremental = "<Enter>";
              scopeIncremental = "grc";
              nodeDecremental = "<BS>";
            };
          };
          moduleConfig = {
            highlight = {
              enable = true;
            };
          };
        };

        lsp = {
          enable = true;
          # Disable highlights from LSP
          # onAttach = ''
          #   client.server_capabilities.semanticTokensProvider = nil
          # '';
          servers = {
            # Nix.
            nil-ls = {
              enable = true;
              settings.nix.flake.autoArchive = true;
            };

            # Python.
            pyright.enable = true;

            # Bash
            bashls.enable = true;

            # Typos.
            typos-lsp = {
              enable = true;
              extraOptions.init_options.diagnosticSeverity = "Hint";
            };

            # Lua.
            lua-ls.enable = true;

            # Clojure
            clojure-lsp.enable = true;

            # Typescript
            tsserver.enable = true;
            eslint.enable = true; # Linter as lsp

            # Markdown
            marksman.enable = true;

            # Haskell.
            hls.enable = true;

            rust-analyzer = {
              enable = true;
              installCargo = false;
              installRustc = false;
            };
          };
          keymaps.lspBuf = {
            "gd" = "definition";
            "gD" = "references";
            "gi" = "implementation";
          };
        };

        gitsigns = {
          enable = true;
          settings = {
            current_line_blame = false;
            signcolumn = false;
          };
        };

        conform-nvim = {
          enable = true;
          extraOptions = {
            lsp_fallback = false;
          };
          formattersByFt = {
            # Conform will run multiple formatters sequentially.
            json = ["jq"];
            sh = ["shfmt"];
            lua = ["stylua"];
            nix = ["alejandra"];
            clojure = ["zprint"];
            haskell = ["fourmolu"];
            python = ["isort" "yapf"];
            javascript = ["prettierd"];
            typescript = ["prettierd"];
            javascriptreact = ["prettierd"];
            typescriptreact = ["prettierd"];
            css = ["prettierd"];
            html = ["prettierd"];
            graphql = ["prettierd"];
            markdown = ["prettierd"];
            # Use the "*" filetype to run formatters on all filetypes.
            "*" = ["trim_whitespace"];
          };
          formatters = {
            cljfmt = {
              command = "${lib.getExe pkgs.cljfmt}";
              args = ["fix" "-"];
              stdin = true;
            };
          };
        };

        lint = {
          enable = true;
          lintersByFt = {
            rst = ["vale"];
            text = ["vale"];
            c = ["clangtidy"];
            cpp = ["clangtidy"];
            haskell = ["hlint"];
            json = ["jsonlint"];
            markdown = ["markdownlint"];
            bash = ["shellcheck"];
            shell = ["shellcheck"];
            clojure = ["clj-kondo"];
            nix = ["nix" "deadnix"];
            dockerfile = ["hadolint"];
          };
        };

        which-key = {
          enable = true;
          ignoreMissing = false;
          registrations = {
            "gd" = "[g]o to [d]efinition";
            "gD" = "[g]o to uses";
            "gi" = "[g]o to [i]mplementation";
            "K" = "[H]over info";
            "<Leader>t" = "+[t]elescope";
            "<Leader>h" = "+[h]arpoon";
            "<leader>ha" = "[h]arpoon [a]dd file";
            "<leader>hm" = "[h]arpoon [m]enu";
            "<leader>hcm" = "[h]arpoon [c]ommand [m]enu";
            "<leader>hn" = "[h]arpoon [n]ext";
            "<leader>hp" = "[h]arpoon [p]revious";
            "<C-h>" = "harpoon file 1";
            "<C-j>" = "harpoon file 2";
            "<C-k>" = "harpoon file 3";
            "<C-l>" = "harpoon file 4";
          };
          plugins = {
            presets = {
              # Needs to be false for indent keybindings
              operators = false; #adds help for operators like d, y, ...";
            };
          };
        };

        oil = {
          enable = true;
          settings.defaultFileExplorer = true;
        };

        luasnip = {
          enable = true;
          extraConfig = {
            enable_autosnippets = true;
            store_selection_keys = "<Tab>";
          };
          fromVscode = [
            {
              lazyLoad = true;
            }
          ];
        };

        cmp = {
          enable = true;
          autoEnableSources = true;
          settings = {
            autocomplete = true;
            performance = {
              debounce = 200;
              throttle = 200;
              maxViewEntries = 5;
              fetchingTimeout = 50;
            };
            snippet.expand = ''
              function(args)
                require('luasnip').lsp_expand(args.body)
              end
            '';
            sources = [
              {name = "nvim_lsp";}
            ];
            mapping = {
              "<Tab>" = "cmp.mapping(cmp.mapping.select_next_item(), {'i', 's'})";
              "<S-Tab>" = "cmp.mapping(cmp.mapping.select_prev_item(), {'i', 's'})";
              "<C-j>" = "cmp.mapping.select_next_item()";
              "<C-k>" = "cmp.mapping.select_prev_item()";
              "<C-e>" = "cmp.mapping.abort()";
              "<C-b>" = "cmp.mapping.scroll_docs(-4)";
              "<C-f>" = "cmp.mapping.scroll_docs(4)";
              "<C-Space>" = "cmp.mapping.complete()";
              "<CR>" = "cmp.mapping.confirm({ select = false })";
              "<S-CR>" = "cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true })";
            };

            window = {
              completion.scrollbar = true;
              documentation.border = "single";
            };
          };
        };
      };

      # }}}
      autoCmd = [
        {
          event = ["BufReadPost"];
          pattern = ["*"];
          command = "normal!'\"";
          desc = "Open at last location";
        }
        {
          event = ["BufEnter"];
          pattern = ["*"];
          command = "setlocal formatoptions-=c formatoptions-=r formatoptions-=o";
          desc = "Dont insert comments on newline";
        }
      ];
    };
    xdg.configFile."neovide/neovide.toml".source = (pkgs.formats.toml {}).generate "neovideExtraConfigDIY" {
      font = {
        normal = ["${config.stylix.fonts.monospace.name}"];
        size = 13;
      };
    };
  };
}
