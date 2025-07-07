{
  lib,
  pkgs,
  config,
  inputs,
  conUsername,
  ...
}: {
  home-manager.users.${conUsername} = {osConfig, ...}: {
    imports = lib.my.withModules (map (x: "neovim/" + x) [
      "myNeovide.nix"

      "plugins/myVimVisualMulti.nix"
      "plugins/mySpider.nix"
      "plugins/myRainbow.nix"
    ]);

    home.sessionVariables = {EDITOR = "nvim";};

    # I put them in the global scope since direnv deletes the one in the neovim scope
    home.packages = [
      pkgs.vim-language-server
      pkgs.deadnix # Nix linter
      pkgs.tinymist # Typst lsp
      pkgs.nodePackages.jsonlint
      pkgs.hlint # Haskell linter
      pkgs.stylua # Lua formatter
      pkgs.shfmt # Shell formatter
      pkgs.statix # Another linter
      pkgs.yapf # Python formatter
      pkgs.black # Python formatter
      pkgs.isort # Python import sorter
      pkgs.prettierd # Javascript formatter
      pkgs.markdownlint-cli # Markdown linter
      pkgs.vscode-langservers-extracted # Web LSPs
      pkgs.haskellPackages.fourmolu # Haskell formatter
      pkgs.nodePackages.prettier # Javascript formatter
    ];

    programs.nixvim = {
      enable = true;

      performance = {
        byteCompileLua.enable = true;
        byteCompileLua.configs = true;
        byteCompileLua.initLua = true;
        byteCompileLua.plugins = true;
        byteCompileLua.nvimRuntime = true;
      };

      extraPlugins = let
        mkNvimplugin = name:
          pkgs.vimUtils.buildVimPlugin {
            inherit name;
            src = builtins.getAttr ("nvim-plugin-" + name) inputs;
          };
      in
        [
          pkgs.vimPlugins.img-clip-nvim
          pkgs.vimPlugins.vim-pencil
          pkgs.vimPlugins.gruvbox-material
          pkgs.vimPlugins.dial-nvim
          pkgs.vimPlugins.typst-preview-nvim
        ]
        ++ map mkNvimplugin [
          "cutlass"
          "faster"
          "telescope-git-file-history"
        ];

      highlightOverride = {
        noCursor.blend = 100;
        statusline.bg = "NONE";
        ModeMsg.fg = "#${osConfig.const.accentColor}";
        MsgArea.fg = "#${osConfig.const.accentColor}";
        statusline.fg = "#${osConfig.const.accentColor}";
        FloatBorder.fg = "#${osConfig.const.accentColor}";
        CursorLineNr.fg = "#${osConfig.const.accentColor}";
        CursorLineNr.bg = "${config.lib.stylix.colors.withHashtag.base01}"; # Gray numberline
        MiniIndentscopeSymbol.fg = "${config.lib.stylix.colors.withHashtag.base01}"; # Gray indentline
      };

      opts = {
        # Speed
        lazyredraw = true;
        regexpengine = 1;

        # Indents.
        tabstop = 2;
        shiftwidth = 2;
        softtabstop = 2;
        expandtab = true;
        # smartindent = true;
        breakindent = true; # Indent when wrapping

        # Wrapping.
        linebreak = true;
        wrap = true;

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
        # foldexpr = "nvim_treesitter#foldexpr()";

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

        gruvbox_material_foreground = "original";
        gruvbox_material_enable_bold = 1;
        gruvbox_material_enable_italic = 1;
        gruvbox_material_transparent_background = 2;
        # Otherwise python sets itself to indent 4
        python_recommended_style = 0;
      };

      extraFiles = {
        "ftplugin/haskell.vim".text = "set nocursorline"; # https://github.com/nvim-treesitter/nvim-treesitter/issues/7967
        "ftplugin/markdown.vim".text = "setlocal wrap";
      };

      extraConfigLua =
        # Lua
        ''
          -- Miscellaneous {{{
          -- 24 bit color.
          if vim.fn.has('termguicolors') == 1 then
            vim.opt.termguicolors = true
          end

          vim.cmd[[colorscheme gruvbox-material]]

          -- Faster syntax highlighting.
          vim.cmd("syntax sync minlines=256")

          -- Hide end of line tildes.
          vim.opt.fillchars:append({ eob = " " })
          -- }}}

          -- Stops treesitter node increment in command window (q:) {{{
          vim.api.nvim_create_augroup("_cmd_win", { clear = true })
          vim.api.nvim_create_autocmd("CmdWinEnter", {
              callback = function()
                  local ok, _ = pcall(vim.keymap.del, "n", "<CR>", { buffer = true })
                  if not ok then
                      -- Silently ignore error when node increment isnt set, like in q/
                  end
              end,
              group = "_cmd_win",
          })
          -- }}}

          vim.cmd[[
            augroup remember_folds
              autocmd!
              au BufWinLeave ?* mkview 1
              au BufWinEnter ?* silent! loadview 1
            augroup END
          ]]

          -- Remember last line {{{
          vim.api.nvim_create_autocmd("BufRead", {
            callback = function(opts)
              vim.api.nvim_create_autocmd("BufWinEnter", {
                once = true,
                buffer = opts.buf,
                callback = function()
                  local ft = vim.bo[opts.buf].filetype
                  local last_known_line = vim.api.nvim_buf_get_mark(opts.buf, '"')[1]
                  if
                    not (ft:match("commit") and ft:match("rebase"))
                    and last_known_line > 1
                    and last_known_line <= vim.api.nvim_buf_line_count(opts.buf)
                  then
                    vim.api.nvim_feedkeys([[g`"]], "nx", false)
                  end
                end,
              })
            end,
          })
          -- }}}

          -- Statusline {{{
          -- Statusline components
          local cmp = {}

          -- Helper function to call statusline components by name
          function _G._statusline_component(name)
            return cmp[name]()
          end

          -- Diagnostic status component
          function cmp.diagnostic_status()
            local ok = '''

            local ignore = {
              ['c'] = true, -- command mode
              ['t'] = true  -- terminal mode
            }

            local mode = vim.api.nvim_get_mode().mode

            if ignore[mode] then
              return ok
            end

            local levels = vim.diagnostic.severity
            local errors = #vim.diagnostic.get(0, { severity = levels.ERROR })
            if errors > 0 then
              return 'ERROR '
            end

            local warnings = #vim.diagnostic.get(0, { severity = levels.WARN })
            if warnings > 0 then
              return 'WARN '
            end

            return ok
          end

          -- Git status component using gitsigns
          function cmp.git_status()
            local git_info = vim.b.gitsigns_status_dict
            if not git_info or git_info.head == "" then
              return ""
            end

            local added = git_info.added and ("%#GitSignsAdd#+" .. git_info.added .. " ") or ""
            local changed = git_info.changed and ("%#GitSignsChange#~" .. git_info.changed .. " ") or ""
            local removed = git_info.removed and ("%#GitSignsDelete#-" .. git_info.removed .. " ") or ""

            -- Clean up display if values are 0
            if git_info.added == 0 then
              added = ""
            end
            if git_info.changed == 0 then
              changed = ""
            end
            if git_info.removed == 0 then
              removed = ""
            end

            return table.concat({
              " ",
              added,
              changed,
              removed,
              "%#GitSignsAdd#branch ",
              git_info.head,
              " %#Normal#",
            })
          end

          -- Define the statusline
          local statusline = {
            '%{%v:lua._statusline_component("diagnostic_status")%}',  -- Diagnostic status
            '%t',                                                    -- File name
            '%r',                                                    -- Read-only flag
            '%m',                                                    -- Modified flag
            '%{%v:lua._statusline_component("git_status")%}',         -- Git status
            '%=',                                                    -- Right align
            '%{&filetype} ',                                         -- Filetype
            '%2p%%',                                                 -- File position in percentage
          }

          -- Set the statusline
          vim.o.statusline = table.concat(statusline, ''')
          -- }}}

          -- Keymaps {{{
          -- Better open
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
          end, {desc = "rename node"})

          -- Open/close quickfix on toggle
          local function toggle_quickfix()
            local quickfix_open = false
            for _, win in ipairs(vim.fn.getwininfo()) do
              if win.quickfix == 1 then
                quickfix_open = true
                break
              end
            end
            if quickfix_open then
              vim.cmd('cclose')
            else
              vim.cmd('copen')
            end
          end
          vim.keymap.set('n', '<S-f>', toggle_quickfix, { silent = true, desc = "Toggle quickfix" })

          -- Jump whitespace
          vim.keymap.set("n", "{", "<Cmd>call search('^\\s*\\S', 'Wbc') | call search('^\\s*$\\|\\%^', 'Wb')<CR>", { desc = "jump whitespace forward"})
          vim.keymap.set("n", "}", "<Cmd>call search('^\\s*\\S', 'Wc') | call search('^\\s*$\\|\\%$', 'W')<CR>", { desc = "jump whitespae backward"})

          -- Keep selection when indenting.
          vim.keymap.set("v", ">", ">gv", { desc = "Keep selection after indenting" })
          vim.keymap.set("v", "<", "<gv", { desc = "Keep selection after unindenting" })

          -- Previous buffer
          vim.keymap.set('n', '<S-B>', '<C-6>')

          -- Split movement
          vim.keymap.set("n", "<S-M-h>", "<cmd>wincmd h<CR>", { desc = "Move to the split on the left side" })
          vim.keymap.set("n", "<S-M-l>", "<cmd>wincmd l<CR>", { desc = "Move to the split on the right side" })
          vim.keymap.set("n", "<S-M-k>", "<cmd>wincmd k<CR>", { desc = "Move to the split above" })
          vim.keymap.set("n", "<S-M-j>", "<cmd>wincmd j<CR>", { desc = "Move to the split below" })

          -- Copy and paste
          vim.keymap.set("n", "<c-v>", '"+p', { desc = "proper paste" })
          vim.keymap.set({"i", "c"}, "<C-V>", "<C-r>+", { desc = "Proper paste" })
          vim.keymap.set({"i", "c"}, "<C-V>", "<C-r>+", { desc = "Proper paste" })
          vim.keymap.set('n', 'Y', 'y$', { desc = "Copy till end of line without newline" })
          vim.keymap.set('n', 'yy', '^y$', { desc = "Copy line without newline and whitespace" })
          vim.keymap.set('v', 'v', '<Esc>^vg_', { desc = "Select line without newline and whitespace" })

          -- Infinite paste
          vim.keymap.set('v', 'p', '"_dP')

          -- Surround each line
          vim.keymap.set('v', 'L', ':norm yss')

          -- Basic
          vim.keymap.set('n', ';', ':', { desc = "Command mode with or without shift" })
          vim.keymap.set("n", ">", ">>", { desc = "Indent more", silent = true })
          vim.keymap.set("n", "<lt>", "<lt><lt>", { desc = "Indent less", silent = true })
          vim.keymap.set("v", ".", "<cmd>normal .<CR>", { desc = "Dot commands over visual blocks" })
          vim.keymap.set("n", "G", "Gzz", { desc = "Center bottom" })
          vim.keymap.set("n", "gg", "ggzz", { desc = "Center top" })
          vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")
          vim.keymap.set("v", "gj", "J", { desc = "join lines" })
          vim.keymap.set("v", "J", ":m '>+1<CR>gv==kgvo<esc>=kgvo", { desc = "move highlighted text down" })
          vim.keymap.set("v", "K", ":m '<-2<CR>gv==jgvo<esc>=jgvo", { desc = "move highlighted text up" })
          vim.keymap.set( "i", "<C-r>", "<C-r><C-o>", { desc = "Insert contents of named register. Inserts text literally, not as if you typed it." })

          -- Tabs
          vim.keymap.set('n', 'tk', ':tabnext<CR>', {silent = true, desc = "Go to next tab" })
          vim.keymap.set('n', 'tj', ':tabprev<CR>', {silent = true, desc = "Go to previous tab" })
          vim.keymap.set('n', 'td', ':tabclose<CR>', {silent = true, desc = "Close current tab" })
          vim.keymap.set('n', '<leader>1', '1gt', {silent = true, desc = "Go to tab 1" })
          vim.keymap.set('n', '<leader>2', '2gt', {silent = true, desc = "Go to tab 2" })
          vim.keymap.set('n', '<leader>3', '3gt', {silent = true, desc = "Go to tab 3" })
          vim.keymap.set('n', '<leader>4', '4gt', {silent = true, desc = "Go to tab 4" })
          vim.keymap.set('n', '<leader>5', '5gt', {silent = true, desc = "Go to tab 5" })
          vim.keymap.set('n', '<leader>6', '6gt', {silent = true, desc = "Go to tab 6" })
          vim.keymap.set('n', '<leader>7', '7gt', {silent = true, desc = "Go to tab 7" })
          vim.keymap.set('n', '<leader>8', '8gt', {silent = true, desc = "Go to tab 8" })
          vim.keymap.set('n', '<leader>9', '9gt', {silent = true, desc = "Go to tab 9" })

          -- Makes ctrl+s increment to not conflict with tmux
          vim.keymap.set('n', '<C-s>', '<C-a>', {silent = true, desc = "Increment number under cursor" })

          -- Center search and substitution
          vim.keymap.set('n', 'n', 'nzz', {silent = true, desc = "Next search result and center" })
          vim.keymap.set('n', 'N', 'Nzz', {silent = true, desc = "Previous search result and center" })
          vim.keymap.set('n', '*', '*zz', {silent = true, desc = "Search word under cursor and center" })
          vim.keymap.set('n', '#', '#zz', {silent = true, desc = "Search word under cursor (reverse) and center" })
          vim.keymap.set('n', 'g*', 'g*zz', {silent = true, desc = "Search partial word under cursor and center" })
          vim.keymap.set('n', 'g#', 'g#zz', {silent = true, desc = "Search partial word under cursor (reverse) and center" })

          -- Autocomplete
          vim.keymap.set("i", "<C-x>", "<C-x><C-o>", { desc = "Autocomplete" })

          vim.keymap.set('n', '<leader>q', vim.cmd.quit, { desc = "Quit"})
          vim.keymap.set('n', '<leader>Q', vim.cmd.only, { desc = "Quit other windows"})
          -- }}}

          -- Plugins {{{

          -- Nvim-tree {{{
          vim.keymap.set("n", "<leader>op", "<cmd>NvimTreeToggle<CR>", {desc = "[o]pen [p]roject"})
          -- }}}

          -- Telescope {{{
          local utils = require "telescope.utils"
          local builtin = require "telescope.builtin"

          vim.keymap.set("n", "<leader>f", "<cmd>Oil<CR>", {desc = "[f]ile browser"})
          vim.keymap.set("n", "K", "<cmd>Lspsaga hover_doc<CR>", {desc = "hover"})
          vim.keymap.set("n", "<leader>a", "<cmd>Lspsaga code_action<CR>", {desc = "code [a]ctions"})
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
          -- }}}

          -- Telescope extensions {{{
          require("telescope").load_extension("git_file_history")
          local gfh = require("telescope").extensions.git_file_history
          vim.keymap.set("n", "<leader>tv", gfh.git_file_history, { desc = "[t]elescope [v]ersions" })
          -- }}}

          -- Conform {{{
          vim.keymap.set("n", "<Leader>c", function()
            require("conform").format({ timeout_ms = 500 })
          end, { desc = "[c]onform" })

          vim.api.nvim_create_user_command("Conform", function()
              require("conform").format({ timeout_ms = 500 })
          end, { desc = "Format using Conform with a 500ms timeout" })
          -- }}}

          -- Leap {{{
          -- Gray out leap
          vim.api.nvim_set_hl(0, 'LeapBackdrop', { link = 'Comment' })
          vim.api.nvim_set_hl(0, 'LeapMatch', {
            fg = 'white', bold = true, nocombine = true,
          })

          -- Hide the (real) cursor when leaping, and restore it afterwards.
          vim.api.nvim_create_autocmd('User', { pattern = 'LeapEnter',
              callback = function()
                vim.cmd.hi('Cursor', 'blend=100')
                vim.opt.guicursor:append { 'a:Cursor/lCursor' }
              end,
            }
          )

          vim.api.nvim_create_autocmd('User', { pattern = 'LeapLeave',
              callback = function()
                vim.cmd.hi('Cursor', 'blend=0')
                vim.opt.guicursor:remove { 'a:Cursor/lCursor' }
              end,
            }
          )
          -- }}}

          -- dial.nvim {{{
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
          -- }}}

          -- img-clip.nvim {{{
            require('img-clip').setup({
              default = {
                -- file and directory options
                dir_path = "assets",
                extension = "png",
                file_name = "%Y-%m-%d-%H-%M-%S",
                use_absolute_path = false,
                relative_to_current_file = false,

                -- template options
                template = "$FILE_PATH",
                url_encode_path = false,
                relative_template_path = true,
                use_cursor_in_template = true,
                insert_mode_after_paste = false,

                -- prompt options
                prompt_for_file_name = true,
                copy_images = true,
            }
          })
          -- }}}

          -- Cutlass (Delete copy registers) {{{
          require("cutlass").setup({
            override_del = true,
            exclude = { "ns", "nS", "nx", "nX", "nxx", "nX", "vx", "vX", "xx", "xX" }, -- Motion plugins rebind this
          })
          -- }}}

          -- Faster.nvim (Speed up big files) {{{
          require("faster").setup({
            behaviours = {
              bigfile = {
                on = true,
                features_disabled = {
                  "illuminate", "matchparen", "lsp", "treesitter",
                  "indent_blankline", "vimopts", "syntax", "filetype"
                },
                -- Files larger than `filesize` are considered big files. Value is in MB.
                filesize = 0.3,
                -- Autocmd pattern that controls on which files behaviour will be applied.
                -- `*` means any file.
                pattern = "*",
              }
            }
          })
          --- }}}

          -- Harpoon {{{
          local harpoon = require("harpoon")
          local harpoon_extensions = require("harpoon.extensions")
          harpoon:extend(harpoon_extensions.builtins.highlight_current_file())
          -- }}}

          -- Gitsigns {{{
          vim.keymap.set("n", "<leader>gsc", "<cmd>Gitsigns toggle_signs<CR>", {desc = "[g]it[s]igns [c]olumn"})
          vim.keymap.set("n", "<leader>gsb", "<cmd>Gitsigns toggle_current_line_blame<CR>", {desc = "[g]it[s]igns [b]lame"})
          -- }}}

          -- LSP {{{
          -- Transparent hover
          vim.api.nvim_set_hl(0, 'NormalFloat', { link = 'Normal', })

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
            source = true,
            float = {
              win_options = {
                winblend = 100
              },
              border = border,
            }
          })
            -- }}}
          -- }}}
        '';

      clipboard.register = "unnamedplus";
      colorschemes.base16.enable = lib.mkForce false;

      keymaps = [
        {
          mode = "n";
          key = "<leader>ha";
          action.__raw = "function() require'harpoon':list():add() end";
        }
        {
          mode = "n";
          key = "<leader>hm";
          action.__raw = "function() require'harpoon'.ui:toggle_quick_menu(require'harpoon':list()) end";
        }
        {
          mode = "n";
          key = "<leader>hn";
          action.__raw = "function() require'harpoon':list():next() end";
        }
        {
          mode = "n";
          key = "<leader>hp";
          action.__raw = "function() require'harpoon':list():prev() end";
        }
        {
          mode = "n";
          key = "<C-h>";
          action.__raw = "function() require'harpoon':list():select(1) end";
        }
        {
          mode = "n";
          key = "<C-j>";
          action.__raw = "function() require'harpoon':list():select(2) end";
        }
        {
          mode = "n";
          key = "<C-k>";
          action.__raw = "function() require'harpoon':list():select(3) end";
        }
        {
          mode = "n";
          key = "<C-l>";
          action.__raw = "function() require'harpoon':list():select(4) end";
        }

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
        {
          action = "<cmd>lua vim.diagnostic.open_float()<CR>";
          key = "<Leader>e";
          options.desc = "Open diagnostic";
        }
        {
          action = '':!awk '{ print length(), $0 | "sort -n | cut -d\\  -f2-" }'<CR><CR>'';
          key = "<Leader>S";
          options.silent = true;
          options.desc = "[S]ort lines by length";
        }
      ];

      # Plugins {{{
      plugins = {
        flash.enable = true;
        direnv.enable = true;
        comment.enable = true;
        fugitive.enable = true;
        vim-surround.enable = true;
        web-devicons.enable = true;
        friendly-snippets.enable = true;

        nvim-tree = {
          enable = true;
          respectBufCwd = true;
          syncRootWithCwd = true;
          updateFocusedFile = {
            enable = true;
            updateRoot = true;
          };
        };

        project-nvim = {
          enable = true;
          enableTelescope = true;
          settings = {
            silent_chdir = false;
            exclude_dirs = ["~/nixos/resources/haskell/*"];
          };
        };

        harpoon = {
          enable = true;
          enableTelescope = true;
        };

        colorizer = {
          enable = true;
          settings = {
            user_default_options.names = false;
            buftypes = ["*" "!prompt" "!popup"];
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
          };
        };

        telescope = {
          enable = true;
          extensions.fzf-native = {
            enable = true;
            settings = {
              fuzzy = true;
              override_file_sorter = true;
              override_generic_sorter = true;
            };
          };
        };

        lspsaga = {
          enable = true;
          symbolInWinbar.enable = true;
          implement.enable = true;
          lightbulb = {
            sign = false;
            enable = false;
            virtualText = false;
          };
        };

        mini = {
          enable = true;
          modules = {
            align = {};
            indentscope = {
              symbol = "│";
              draw.delay = 0;
              draw.priority = 2;
              options.border = "top";
              options.try_as_border = true;
              options.indent_at_cursor = true;
            };
          };
        };

        trouble = {
          enable = true;
          settings.auto_close = true;
        };

        treesitter = let
          boolMatch = let
            checkPassed =
              lib.assertMsg
              (builtins.match "abc" "abc" == [] && builtins.match "foo" "abc" == null)
              "builtins.match must have changed";
          in
            regex: str: (builtins.match regex str) != null && checkPassed;

          inherit (pkgs.vimPlugins.nvim-treesitter.passthru) allGrammars;
          matchCommentGrammar = str: boolMatch ".*comment-grammar.*" str;
          filteredGrammars = builtins.filter (set: !matchCommentGrammar set.name) allGrammars;
        in {
          enable = true;
          folding = true;
          nixvimInjections = true;
          nixGrammars = true; # Install grammars with Nix
          grammarPackages = filteredGrammars;
          settings = {
            indent.enable = true;
            ignore_install = ["comment"]; # Comment parser is very slow
            auto_install = false;
            highlight.enable = true;
            incremental_selection = {
              enable = true;
              keymaps = {
                scope_incremental = "gsi";
                node_decremental = "<BS>";
                node_incremental = "<Enter>";
                init_selection = "<Enter>";
              };
            };
          };
        };

        lsp = {
          enable = true;
          servers = {
            # Nix.
            nil_ls = {
              enable = true;
              settings.nix.flake.autoArchive = true;
            };
            nixd = {
              # Nix LS
              enable = true;
              settings = let
                flake = ''(builtins.getFlake "${inputs.self}")'';
              in {
                nixpkgs.expr = "import ${flake}.inputs.nixpkgs { }";
                options = rec {
                  nixos.expr = "${flake}.nixosConfigurations.nixos.options";
                  home-manager.expr = "${nixos.expr}.home-manager.users.type.getSubOptions []";
                  nixvim.expr = ''${flake}.packages.${pkgs.system}.nvim.options'';
                };
              };
            };

            # Python.
            pyright.enable = true;

            # Bash
            bashls.enable = true;

            # Typst
            tinymist.enable = true;

            # Typos.
            typos_lsp = {
              enable = true;
              extraOptions.init_options.diagnosticSeverity = "Hint";
            };

            # Lua.
            lua_ls.enable = true;

            # Web
            html.enable = true;
            jsonls.enable = true;
            cssls.enable = true;
            eslint.enable = true;
            ts_ls = {
              enable = true;
              extraOptions.root_dir = ''
                function (filename, bufnr)
                  local util = require 'lspconfig.util'
                  local denoRootDir = util.root_pattern("deno.json", "deno.jsonc")(filename);
                  if denoRootDir then
                    return nil;
                  end
                  return util.root_pattern("package.json")(filename);
                end
              '';
              extraOptions = {
                single_file_support = false;
              };
            };
            denols = {
              enable = true;
              extraOptions.root_dir = ''
                function (filename, bufnr)
                  local util = require 'lspconfig.util'
                  return util.root_pattern("deno.json", "deno.jsonc")(filename);
                end
              '';
            };

            # Haskell.
            hls = {
              installGhc = false;
              enable = true;
            };

            rust_analyzer = {
              enable = true;
              installCargo = false;
              installRustc = false;
            };
          };
          keymaps.lspBuf = {
            "<leader>gd" = "definition";
            "<leader>gD" = "references";
            "<leader>gi" = "implementation";
          };
        };

        gitsigns = {
          enable = true;
          settings = {
            current_line_blame = false;
            signcolumn = true;
          };
        };

        conform-nvim = {
          enable = true;
          settings = {
            lsp_fallback = false;
            formatters_by_ft = {
              # Conform will run multiple formatters sequentially.
              json = ["jq"];
              jsonc = ["prettierd"];
              sh = ["shfmt"];
              lua = ["stylua"];
              nix = ["alejandra"];
              haskell = ["fourmolu"];
              graphql = ["prettierd"];
              markdown = ["prettierd"];
              python = ["isort" "yapf"];
              css = ["prettierd"];
              html = ["prettierd"];
              scss = ["prettierd"];
              javascript = ["prettierd"];
              javascriptreact = ["prettierd"];
              typescript = ["prettierd"];
              typescriptreact = ["prettierd"];
              # Use the "*" filetype to run formatters on all filetypes.
              "*" = [
                "squeeze_blanks"
                "trim_whitespace"
                "trim_newlines"
              ];
            };
            formatters = {
              cljfmt = {
                command = "${lib.getExe pkgs.cljfmt}";
                args = ["fix" "-"];
                stdin = true;
              };
              shfmt.args = lib.mkOptionDefault ["-i" "2"];
              squeeze_blanks = {
                command = pkgs.lib.getExe' pkgs.coreutils "cat";
              };
            };
          };
        };

        lint = let
          statixConfig = builtins.toFile "statix.toml" ''disabled = [repeated_keys]'';
        in {
          enable = true;
          linters.statix.args = ["--config=${statixConfig}"];
          lintersByFt = {
            c = ["clangtidy"];
            cpp = ["clangtidy"];
            haskell = ["hlint"];
            json = ["jsonlint"];
            bash = ["shellcheck"];
            shell = ["shellcheck"];
            nix = ["nix" "deadnix" "statix"];
            dockerfile = ["hadolint"];
            markdown = ["markdownlint"];
          };
        };

        which-key = {
          enable = true;
          settings = {
            delay = 1000;
            win.border = "single";
            spec = let
              register = key: text: icon: {
                __unkeyed = key;
                group = text;
                inherit icon;
              };
            in [
              (register "<leader>t" "Telescope" " ")
              (register "<leader>s" "Swap" " ")
              (register "<leader>o" "Open" " ")
              (register "<leader>r" "Re" " ")
              (register "<leader>q" "Quit" "󱢓 ")
              (register "<leader>d" "Definition" "")
              (register "<leader>c" "Conform" " ")
              (register "<leader>f" "File browser" " ")
              (register "<leader>S" "Sort by length" "󰒼 ")
              (register "<S-k>" "Hover info" "")

              (register "<leader>g" "go" "󰜎 ")
              (register "<leader>gs" "Gitsigns" " ")
              (register "<leader>gd" "go to definition" "")
              (register "<leader>gr" "go to references" "")
              (register "<leader>gi" "go to implementation" "")

              (register "<leader>h" "Harpoon" "󱢓 ")
              (register "<leader>ha" "Add file" "")
              (register "<leader>hm" "File menu" "")
              (register "<leader>hc" "Command menu" "")
              (register "<leader>hn" "Next file" "")
              (register "<leader>hp" "Previous file" "")
            ];
          };
        };

        oil = {
          enable = true;
          settings = {
            defaultFileExplorer = true;
            delete_to_trash = true;
          };
        };

        luasnip = {
          enable = true;
          settings = {
            enable_autosnippets = true;
            store_selection_keys = "<Tab>";
          };
        };

        cmp = {
          enable = true;
          autoEnableSources = true;
          settings = {
            autocomplete = true;
            sources = [{name = "nvim_lsp";}];
            performance = {
              debounce = 200;
              throttle = 200;
              maxViewEntries = 5;
              fetchingTimeout = 100;
            };
            snippet.expand = ''
              function(args)
                require('luasnip').lsp_expand(args.body)
              end
            '';
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
          event = ["BufEnter"];
          pattern = ["*"];
          command = "setlocal formatoptions-=c formatoptions-=r formatoptions-=o";
          desc = "Dont insert comments on newline";
        }
      ];
    };
  };
}
