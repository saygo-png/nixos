" Basic
if has('termguicolors')
 set termguicolors
endif
set syntax=on
filetype plugin on
filetype indent on
set number relativenumber
set wildmenu
set wildmode=longest,list,full
set title
set clipboard+=unnamedplus
set mouse=
set ignorecase
set smartcase
set showmode
set showcmd
set nocompatible
set signcolumn=number
set noswapfile
set hidden

" Any
let g:python_recommended_style=0
set formatoptions=l
set tabstop=2
set softtabstop=-1
set shiftwidth=2
set expandtab
set splitbelow splitright
set shiftround
set nojoinspaces
set cmdheight=0
set history=500
set wildmenu

set cursorline
set showmatch
set cpoptions+=I
set complete-=i
set completeopt-=preview
set nrformats-=octal
"set notimeout
" Speed to switch to normal mode
set ttimeoutlen=0
set incsearch
set laststatus=2
set scrolloff=3
set sidescrolloff=10
set display+=lastline
set display+=truncate
set viminfo^=!
set viewoptions-=options
set nolangremap
set sessionoptions-=options
set fillchars=fold:\ ,vert:\│,eob:\ ,msgsep:‾
set backspace=indent,eol,start
set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
nnoremap <silent><F2> :setlocal listchars=eol:¬,tab:>·,trail:~,extends:>,precedes:<,space:␣,nbsp:+<CR>:IndentBlanklineDisable<CR>
nnoremap <silent><F3> :setlocal listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+<CR>:IndentBlanklineEnable<CR>
setglobal tags-=./tags tags-=./tags; tags^=./tags;
if !exists('g:is_posix') && !exists('g:is_bash') && !exists('g:is_kornshell') && !exists('g:is_dash')
 let g:is_posix = 1
endif" Syntax and filetype specific indentation and plugins on.
set virtualedit+=onemore
" Faster syntax highlight.
syntax sync minlines=256

" Shut up.
set noerrorbells
set visualbell
let g:loaded_python3_provider = 0
let g:loaded_perl_provider = 0
let g:loaded_node_provider = 0
let g:loaded_ruby_provider = 0

" Convert to unix filetype on save.
set ff=unix
set encoding=utf-8
set fileencoding=utf-8

" Search.
set hlsearch
set shortmess-=S " Show amount of search results

"  Highlight toggle.
nnoremap <silent><expr> <Tab> (&hls && v:hlsearch ? ':nohls' : ':set hls')."\n"

" Wrap.
set nowrap
"set showbreak=>
"set wrap
set breakindent
set breakindentopt=sbr
" I use a unicode curly array with a <backslash><space>
set showbreak=↘⠀

" Use ripgrep with fzz as :grep
if executable('rg')
  set grepprg=rg\ --vimgrep
endif

"""""""""""""""""""""""""""""""
" Remaps, binds, keymaps, keybindings.
"""""""""""""""""""""""""""""""

" Center and move thru wrapped lines
noremap j gj
noremap k gk
nnoremap G Gzz
nnoremap gg ggzz

" Indent.
nnoremap <silent> > >>
nnoremap <silent> < <<
" Blank or empty line jump
noremap { <Cmd>call search('^\s*\S', 'Wbc') \| call search('^\s*$\\|\%^', 'Wb')<CR>
noremap } <Cmd>call search('^\s*\S', 'Wc') \| call search('^\s*$\\|\%$', 'W')<CR>

" Running.
nnoremap <silent> gr :w<CR>:Dispatch<CR>
 " Run processing.
autocmd Filetype arduino nnoremap <buffer> gr :w<CR>:!"$HOME"/builds/processing-4.3/processing-java --sketch="$HOME/%:h" --run &<CR>
autocmd Filetype arduino nnoremap <buffer> gR :w<CR>:!"$HOME"/builds/processing-4.3/processing-java --sketch="$HOME/%:h" --present &<CR>
 " Run python.
autocmd Filetype python nnoremap <buffer> gr :w<CR>:AbortDispatch<CR>:Dispatch! python3 "%:p" &<CR>
nnoremap <silent> zz zzI<Esc><CMD>FindCursor #7d8618 500<CR>

 " Run haskell.
autocmd Filetype haskell nnoremap <buffer> gr :w<CR>:AbortDispatch<CR>:Dispatch cabal run &<CR>
nnoremap <silent> zz zzI<Esc><CMD>FindCursor #7d8618 500<CR>

" Next/previous quickfix result
nnoremap <silent> <C-n> :silent cnext<CR>
nnoremap <silent> <C-p> :silent cprevious<CR>

" Any.
let mapleader=","
"nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>
"snoremap <C-U> <C-G>u<C-U>
"snoremap <C-W> <C-G>u<C-W>
"vnoremap <C-U> <C-G>u<C-U>
"vnoremap <C-W> <C-G>u<C-W>
"inoremap <C-U> <C-G>u<C-U>
"inoremap <C-W> <C-G>u<C-W>

" Perform dot commands over visual blocks:
vnoremap . :normal .<CR>

" Tabs.
nnoremap tk :tabnext<CR>
nnoremap tj :tabprev<CR>
nnoremap td :tabclose<CR>
nnoremap <leader>1 1gt
nnoremap <leader>2 2gt
nnoremap <leader>3 3gt
nnoremap <leader>4 4gt
nnoremap <leader>5 5gt
nnoremap <leader>6 6gt
nnoremap <leader>7 7gt
nnoremap <leader>8 8gt
nnoremap <leader>9 9gt

" Makes o insert a blank line in normal mode.
nnoremap o o<Esc>0"_D
nnoremap O O<Esc>0"_D
nnoremap L .

" Makes d delete and x cut, paste is infinite. default vim is retarded
nnoremap d "_d
nnoremap D "_D
vnoremap d "_d
nnoremap dd "_dd
noremap c "_c
vnoremap c "_c
nnoremap C "_C
vnoremap <expr> p 'pgvy'
nnoremap <leader>d ""d
nnoremap <leader>D ""D
vnoremap <leader>d ""d

" No need to press shit for command mode.
vnoremap ; :
vnoremap : ;
nnoremap ; :
nnoremap : ;

lua << EOF
-- Keep selection when indenting.
vim.keymap.set("v", ">", ">gv", { desc = "Keep selection after indenting" })
vim.keymap.set("v", "<", "<gv", { desc = "Keep selection after unindenting" })

-- Window switching.
vim.keymap.set("n", "<C-h>", ":wincmd h<CR>", { desc = "Move to the split on the left side" })
vim.keymap.set("n", "<C-l>", ":wincmd l<CR>", { desc = "Move to the split on the right side" })
vim.keymap.set("n", "<C-k>", ":wincmd k<CR>", { desc = "Move to the split above" })
vim.keymap.set("n", "<C-j>", ":wincmd j<CR>", { desc = "Move to the split below" })
-- Close buffer
vim.keymap.set("n", "Q", ":close<CR>", { desc = "Close the current buffer" })
EOF

" Makes ctrl+s increment to not conflict with tmux.
nnoremap <C-s> <C-a>
set omnifunc=syntaxcomplete#Complete

" Center search and substitution.
" This is also configued by a plugin in the plugins section.
nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zzo
com! -nargs=* -complete=command ZZWrap let &scrolloff=999 | exec <q-args> | let &so=0
noremap <Leader>s "sy:ZZWrap .,$s/<C-r>s//gc<Left><Left><Left>

" Open urls in .
function! GetVisualSelection()
 if mode()=="v"
  let [line_start, column_start] = getpos("v")[1:2]
  let [line_end, column_end] = getpos(".")[1:2]
 else
  let [line_start, column_start] = getpos("'<")[1:2]
  let [line_end, column_end] = getpos("'>")[1:2]
  end
  if (line2byte(line_start)+column_start) > (line2byte(line_end)+column_end)
   let [line_start, column_start, line_end, column_end] =
       \   [line_end, column_end, line_start, column_start]
   end
   let lines = getline(line_start, line_end)
   if len(lines) == 0
    return ['']
   endif
   if &selection ==# "exclusive"
    let column_end -= 1 "Needed to remove the last character to make it match the visual selction
   endif
   if visualmode() ==# "\<C-V>"
    for idx in range(len(lines))
     let lines[idx] = lines[idx][: column_end - 1]
     let lines[idx] = lines[idx][column_start - 1:]
    endfor
   else
    let lines[-1] = lines[-1][: column_end - 1]
    let lines[ 0] = lines[ 0][column_start - 1:]
   endif
   return join(lines)  "returns selection as a string of space seperated line
  endfunction
vnoremap <leader>o :<BS><BS><BS><BS><BS>execute '!openlisturl' shellescape(GetVisualSelection())<CR>

"""""""""""""""""""""""""""""""
" Autocommands, autocommand, au.
"""""""""""""""""""""""""""""""
lua << EOF
vim.api.nvim_create_autocmd({'FileType'}, {
  desc = 'keymap \'q\' to close help/quickfix/netrw/etc windows',
  pattern = 'help,qf,netrw',
  callback = function()
   vim.keymap.set('n', 'Q', '<C-w>c', {buffer = true, desc = 'Quit (or Close) help, quickfix, netrw, etc windows', })
  end
})


EOF
" Open file at last closed location. (this is literal magic)
autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
autocmd BufReadPost norm zz

" Change normal mode cursor to underline.
"set guicursor=n-v-c-sm:hor100,i-ci-ve:ver25,r-cr-o:hor20"

" Automatically deletes all trailing whitespace and newlines at end of file on save.
"autocmd BufWritePre * %s/\s\+$//e
"autocmd BufWritePre * %s/\n\+\%$//e
"autocmd BufWritePre *.[ch] %s/\%$/\r/e " add trailing newline for ANSI C standard
"autocmd BufWritePre * %retab!

" Run xrdb whenever Xdefaults or Xresources are updated.
autocmd BufRead,BufNewFile Xresources,Xdefaults,xresources,xdefaults set filetype=xdefaults
autocmd BufWritePost Xresources,Xdefaults,xresources,xdefaults !xrdb %

" Hide cursor when unfocused.
let my_cursor_style = &guicursor
augroup cursorline
  autocmd!
  autocmd FocusGained,WinEnter * let &guicursor = my_cursor_style
  autocmd FocusGained,WinEnter * setlocal cursorline
  autocmd FocusLost,WinLeave * setlocal nocursorline guicursor=a:noCursor/lCursor
augroup END

" Vanilla plugins?
runtime macros/matchit.vim
packadd cfilter
" Plug.vim -- all my plugin configuration is below,
" frozen makes the plugins not update.
" Auto install plug
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
call plug#begin('~/.config/nvim/plugged')
 Plug 'iamcco/markdown-preview.nvim'                    ,{ 'frozen': 1, 'do': 'cd app && npx --yes yarn install' }
 Plug 'nvim-treesitter/nvim-treesitter'                 ,{ 'frozen': 0, 'do': ':TSUpdate' }
  Plug 'nvim-treesitter/nvim-treesitter-context'        ,{ 'frozen': 0 }

" Plug 'ellisonleao/gruvbox.nvim'                        ,{ 'frozen': 0 }
 Plug 'luisiacc/gruvbox-baby'                           ,{ 'frozen': 1 }
 Plug 'sainnhe/gruvbox-material'                        ,{ 'frozen': 1 }
 " Plug 'lukas-reineke/indent-blankline.nvim'             ,{ 'frozen': 1, 'tag': 'v2.20.8' }
 Plug 'echasnovski/mini.indentscope'                    ,{ 'frozen': 1 }
 Plug 'tomtom/tcomment_vim'                             ,{ 'frozen': 1 }
 Plug 'tpope/vim-dispatch'                              ,{ 'frozen': 1 }
" Plug 'psliwka/vim-smoothie'                           ,{ 'frozen': 1 }

 " Unimportant plugins.
 Plug 'tpope/vim-surround'                              ,{ 'frozen': 1 }
 Plug 'HiPhish/rainbow-delimiters.nvim'                 ,{ 'frozen': 0 }
 Plug 'folke/which-key.nvim'                            ,{ 'frozen': 1 }
 Plug 'echasnovski/mini.align'                          ,{ 'frozen': 1 }
 Plug 'nvim-telescope/telescope.nvim'                   ,{ 'frozen': 1 }
  Plug 'nvim-lua/plenary.nvim'                          ,{ 'frozen': 1 }
  Plug 'nvim-telescope/telescope-fzf-native.nvim'       ,{ 'frozen': 1, 'do': 'make' }
  Plug 'MunifTanjim/nui.nvim'                           ,{ 'frozen': 1 }
 Plug 'tpope/vim-eunuch'                                ,{ 'frozen': 1 }
 Plug 'preservim/nerdtree'                              ,{ 'frozen': 1 }
 Plug 'ggandor/leap.nvim'                               ,{ 'frozen': 1 }
 Plug 'monaqa/dial.nvim'                                ,{ 'frozen': 1 }
 Plug 'axlebedev/vim-find-my-cursor'                    ,{ 'frozen': 1 }
 Plug 'brenoprata10/nvim-highlight-colors'              ,{ 'frozen': 1 }
 Plug 'junegunn/vim-slash'                              ,{ 'frozen': 1 }
 Plug 'Eandrju/cellular-automaton.nvim'                 ,{ 'frozen': 1 }
 Plug 'kevinhwang91/nvim-ufo'                           ,{ 'frozen': 1 }
  Plug 'kevinhwang91/promise-async'                     ,{ 'frozen': 1 }
" Plug 'andrewferrier/wrapping.nvim'                     ,{ 'frozen': 1 }

 " Autocomplete
 Plug 'hrsh7th/cmp-nvim-lsp'                            ,{ 'frozen': 0 }
Plug 'hrsh7th/cmp-buffer'                               ,{ 'frozen': 0 }
Plug 'hrsh7th/cmp-path'                                 ,{ 'frozen': 0 }
Plug 'hrsh7th/cmp-cmdline'                              ,{ 'frozen': 0 }
Plug 'hrsh7th/nvim-cmp'                                 ,{ 'frozen': 0 }
Plug 'hrsh7th/cmp-vsnip'                                ,{ 'frozen': 0 }
Plug 'saadparwaiz1/cmp_luasnip'                         ,{ 'frozen': 0 }
Plug 'L3MON4D3/LuaSnip'                                 ,{ 'frozen': 0 }

" LSP
" Plug 'MrcJkb/haskell-tools.nvim'                       ,{ 'frozen': 0 }
 Plug 'williamboman/mason.nvim'                         ,{ 'frozen': 0 }
  Plug 'williamboman/mason-lspconfig.nvim'              ,{ 'frozen': 0 }
  Plug 'neovim/nvim-lspconfig'                          ,{ 'frozen': 0 }
 Plug 'nvimtools/none-ls.nvim'                          ,{ 'frozen': 0 }
  Plug 'jay-babu/mason-null-ls.nvim'                    ,{ 'frozen': 0 }
call plug#end()
"" More in the plugins section.

" Gruvbox
set background=dark
let g:gruvbox_material_enable_bold = 1
let g:gruvbox_material_enable_italic = 1
let g:gruvbox_material_enable_underline = 0
let g:gruvbox_material_current_word = 'underline'
let g:gruvbox_material_statusline_style = 'original'
let g:gruvbox_material_foreground = 'original'
let g:gruvbox_material_dim_inactive_windows = 1
let g:gruvbox_material_transparent_background = 2
let g:gruvbox_material_float_style = 'dim'
let g:gruvbox_material_diagnostic_line_highlight = 1
colorscheme gruvbox-material

"hi Normal guibg=NONE ctermbg=NONE
hi statusline ctermbg=NONE guibg=NONE gui=none guifg=#7d8618
hi LineNr guifg=#7d8618
hi noCursor blend=100 cterm=strikethrough
hi ModeMsg guifg=#7d8618
hi MsgArea guifg=#7d8618
"
"" for transparent background
"highlight clear CursorLine
"highlight clear Folded
"highlight clear NonText
"hi clear LineNr
"hi clear CursorLineNr
"hi Normal ctermbg=none guibg=none gui=none
"hi LineNr ctermbg=none guifg=#7d8618 gui=none
"hi Folded ctermbg=none guibg=none gui=none
"hi NonText ctermbg=none guibg=none gui=none
"hi SpecialKey ctermbg=none guibg=none gui=none
"hi VertSplit ctermbg=none guibg=none gui=none
"hi SignColumn ctermbg=none guibg=none gui=none
"hi CursorColumn cterm=NONE ctermbg=NONE ctermfg=NONE gui=none
"hi CursorLine cterm=NONE ctermbg=NONE ctermfg=NONE gui=none
hi CursorLineNr cterm=NONE ctermbg=NONE ctermfg=NONE gui=none guifg=#fabd2f

" Tcomment.
" Comment at start of line.
let g:tcomment#options ={
\ 'whitespace': 'no',
\ 'strip_whitespace': '0'
\}

" MiniAlign.
lua << EOF
 require('mini.align').setup()
EOF

" Identscope.
lua << EOF
 require('mini.indentscope').setup({
  draw = {
   delay = 0,
   priority = 2,
  },
  symbol = '│',
  options = {
   -- Type of scope's border: which line(s) with smaller indent to
   -- categorize as border. Can be one of: 'both', 'top', 'bottom', 'none'.
   border = 'top',

   -- Whether to use cursor column when computing reference indent.
   -- Useful to see incremental scopes with horizontal cursor movements.
   indent_at_cursor = true,

   -- Whether to first check input line to be a border of adjacent scope.
   -- Use it if you want to place cursor on function header to get scope of
   -- its body.
   try_as_border = true,
  },
 })
 vim.cmd [[highlight MiniIndentscopeSymbol guifg=#79740e gui=nocombine]]
 -- Ts comments.
 vim.g.skip_ts_context_commentstring_module = false
EOF
"
" Indentblankline (legacy).
"let g:indent_blankline_char = '│'
"lua << EOF
" vim.opt.list = true
" vim.cmd [[highlight IndentBlanklineIndent1 guifg=#79740e gui=nocombine]]
" vim.cmd [[highlight IndentBlanklineIndent2 guifg=#b57614 gui=nocombine]]
" vim.cmd [[highlight IndentBlanklineIndent3 guifg=#076678 gui=nocombine]]
" vim.cmd [[highlight IndentBlanklineIndent4 guifg=#8f3f71 gui=nocombine]]
" vim.cmd [[highlight IndentBlanklineIndent5 guifg=#427b58 gui=nocombine]]
" vim.cmd [[highlight IndentBlanklineIndent6 guifg=#af3a03 gui=nocombine]]
" require("indent_blankline").setup {
"  space_char_blankline = "",
"  -- show_current_context = true,
"  -- show_current_context_start = true,
"  char_highlight_list =
"  {
"   "IndentBlanklineIndent1",
"   "IndentBlanklineIndent2",
"   "IndentBlanklineIndent3",
"   "IndentBlanklineIndent4",
"   "IndentBlanklineIndent5",
"   "IndentBlanklineIndent6",
"  },
" }
"EOF
" Markdownpreview plug.
let g:mkdp_auto_start = 0
let g:mkdp_auto_close = 1
let g:mkdp_page_title = 'MarkdownPreview'
let g:mkdp_theme = 'light'
" Open the URL in a new Firefox window
let g:mkdp_browserfunc = 'MarkdownPreview'
function! MarkdownPreview(url)
 silent exec "!librewolf --new-window " . shellescape(a:url)
endfunction

" Colors plug.
lua << EOF
 require('nvim-highlight-colors').setup {
 render = 'background', -- or 'foreground' or 'first_column'
  enable_named_colors = true,
  enable_tailwind = true,
 }
EOF

" Search plug.
noremap <plug>(slash-after) zz
if has('timers')
  " Blink 2 times with 50ms interval.
  noremap <expr> <plug>(slash-after) 'zz'.slash#blink(5, 50)
endif

" Extended increment, dial.nvim plug.
lua << EOF
local augend = require("dial.augend")
require("dial.config").augends:register_group{
-- default augends used when no group name is specified
default = {
 augend.integer.alias.decimal,   -- nonnegative decimal number (0, 1, 2, 3, ...)
 augend.integer.alias.hex,       -- nonnegative hex number  (0x01, 0x1a1f, etc.)
 augend.date.alias["%Y/%m/%d"],  -- date (2022/02/19, etc.)
 augend.constant.alias.bool,    -- boolean value (true <-> false)
 augend.semver.alias.semver
 }
}
EOF
nmap  <C-s>  <Plug>(dial-increment)
nmap  <C-x>  <Plug>(dial-decrement)
nmap g<C-s> g<Plug>(dial-increment)
nmap g<C-x> g<Plug>(dial-decrement)
vmap  <C-s>  <Plug>(dial-increment)
vmap  <C-x>  <Plug>(dial-decrement)
vmap g<C-s> g<Plug>(dial-increment)
vmap g<C-x> g<Plug>(dial-decrement)

"""""""""""""""""""""""""""""""
" Language server, lsp.
"""""""""""""""""""""""""""""""

" Mason
lua << EOF
local servers = {
'clangd',
'rust_analyzer',
'bashls',
'cssls',
'html',
'clangd',
'lua_ls',
'jsonls',
'marksman',
--'hls', --dont add with haskell-tools nvim
}

require("mason").setup({
  ensure_installed = { servers, "ruff", "pylsp", },
  ui = {check_outdated_packages_on_open = false},
  automatic_installation = true,
})

local pylsp = require("mason-registry").get_package("python-lsp-server")
pylsp:on("install:success", function()
  local function mason_package_path(package)
    local path = vim.fn.resolve(vim.fn.stdpath("data") .. "/mason/packages/" .. package)
    return path
  end

  local path = mason_package_path("python-lsp-server")
  local command = path .. "/venv/bin/pip"
  local args = {
    "install",
    "pylsp-rope",
    "python-lsp-black",
    "python-lsp-ruff",
    "sqlalchemy-stubs",
    "pylsp-mypy",
    "pyls-memestra",
    "mccabe",
    "mypy",
  },
  require("plenary.job")
    :new({
      command = command,
      args = args,
      cwd = path,
    })
    :start()
 end)

-- Formatters.
require("mason-null-ls").setup({
 ensure_installed = {
  "stylua",
  "jq",
  "proselint",
  "shellcheck",
  "shfmt",
  "luasnip",
  "cppcheck",

  --python
  "yapf",
  "pyflakes",
  "pylint",
  "isort",
  "mypy",
  "pydocstyle",
  "flake8",
  "yapf",

  "cppcheck",
  "hlint",
  "fourmolu",
 }
})

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

local capabilities = require("cmp_nvim_lsp").default_capabilities()
local lspconfig = require('lspconfig')

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
local handlers = {
    ['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, { border = border }),
    ['textDocument/signatureHelp'] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = border }),
}


lspconfig.pylsp.setup {
 settings = {
  pylsp = {
   plugins = {
    -- formatter options
    black = { enabled = false },
    autopep8 = { enabled = false },
    yapf = { enabled = false },
    -- linter options
    pylint = { enabled = false, executable = "pylint" },
    pyflakes = { enabled = true },
    pycodestyle = {
     enabled = true,
     ignore = {'E302', 'E305', 'E111', 'E126', 'E226'},
     maxLineLength = 120,
     indentSize = 2,
    },
    -- type checker
    pylsp_mypy = { enabled = true },
    -- auto-completion options
    rope_autoimport = { enabled = true},
    rope_completion = { enabled = true},
    --jedi_completion = { enabled = true, fuzzy = true, include_params = true },
    jedi_definition = { enabled = true, follow_imports = true, },
    jedi_hover = { enabled = true },
    jedi_references = { enabled = true },
    jedi_signature_help = { enabled = true },
    jedi_symbols = { enabled = true, all_scopes = true },
    mccabe = { enabled = true },
    -- import sorting
    pyls_isort = { enabled = true },
    flake8 = { enabled = false}
   },
  },
 },
 handlers = handlers,
 on_init = function(client)
  client.server_capabilities.documentFormattingProvider = false
 end,
}

lspconfig.hls.setup({
 capabilities = capabilities,
 handlers = handlers,
})

lspconfig.ruff.setup({
 capabilities = capabilities,
  cmd = { "ruff", "server", "--preview", "--config", vim.fn.expand("$XDG_CONFIG_HOME/ruff/ruff.toml")},
-- on_init = function(client)
--  client.server_capabilities.documentFormattingProvider = false
-- end,
 handlers = handlers,
})

for _, lsp in ipairs(servers) do
  lspconfig[lsp].setup {
    -- on_attach = my_custom_on_attach,
    capabilities = capabilities,
    handlers = handlers,
  }
end

-- Cmp colors.
vim.api.nvim_set_hl(0, "CmpNormal", {background = "#98971a"})
vim.api.nvim_set_hl(0, 'CmpItemAbbrMatch', { bg='NONE', fg='#fabd2f' })
vim.api.nvim_set_hl(0, 'CmpItemAbbrMatchFuzzy', { link='CmpIntemAbbrMatch' })
vim.cmd("highlight Pmenu guibg=NONE")


-- luasnip setup
local luasnip = require 'luasnip'
-- nvim-cmp setup
local cmp = require 'cmp'
cmp.setup{
completion = {
 autocomplete = false,
 },
 performance = {
  debounce = 50,
  --throttle = 60,
  --fetching_timeout = 10,
  --max_view_entries = 5,
  },
 enabled = function()
 -- disable completion in comments
 local context = require 'cmp.config.context'
 -- keep command mode completion enabled when cursor is in a comment
 if vim.api.nvim_get_mode().mode == 'c' then
  return true
 else
  return not context.in_treesitter_capture("comment")
  and not context.in_syntax_group("Comment")
  end
  end,
  snippet = {
   expand = function(args)
   luasnip.lsp_expand(args.body)
   end,
   },
    window = {
      completion = { border = border, highlight = "CmpNormal"},
      documentation = { border = border },
    },
  mapping = cmp.mapping.preset.insert({
  ["<C-u>"] = cmp.mapping.scroll_docs(-4), -- Up
  ["<C-d>"] = cmp.mapping.scroll_docs(4), -- Down
  -- C-b (back) C-f (forward) for snippet placeholder navigation.
  ["<Tab>"] = cmp.mapping.complete(),
  ["<CR>"] = cmp.mapping(function(fallback)
  if cmp.visible() then
   if luasnip.expandable() then
    luasnip.expand()
   else
    cmp.confirm({
    select = true,
    })
    end
   else
    fallback()
    end
    end),

    ["<C-j>"] = cmp.mapping(function(fallback)
    if cmp.visible() then
     cmp.select_next_item()
    elseif luasnip.locally_jumpable(1) then
     luasnip.jump(1)
    else
     fallback()
     end
     end, { "i", "s" }),

     ["<C-k>"] = cmp.mapping(function(fallback)
     if cmp.visible() then
      cmp.select_prev_item()
     elseif luasnip.locally_jumpable(-1) then
      luasnip.jump(-1)
     else
      fallback()
      end
      end, { "i", "s" }),
  }),
  sources = {
   { name = "nvim_lsp" },
   { name = "luasnip" },
   { name = "nvim_lua" },
   { name = "nvim_lsp_signature_help" },
   { name = "buffer" },
   },
}
cmp.setup.cmdline('/', {
  view = {
    entries = {name = 'wildmenu', separator = '|' }
  },
})
-- Use LspAttach autocommand to only map the following keys after the language server attaches to the current buffer
vim.api.nvim_create_autocmd('LspAttach', {
 group = vim.api.nvim_create_augroup('UserLspConfig', {}),
 callback = function(ev)
 -- Buffer local mappings.
 -- See `:help vim.lsp.*` for documentation on any of the below functions
 local opts = { buffer = ev.buf }
 vim.keymap.set('n', '<space>d', vim.lsp.buf.definition, opts)
 vim.keymap.set('n', '<space>D', vim.lsp.buf.declaration, opts)
 vim.keymap.set('n', '<space>gd', vim.lsp.buf.type_definition, opts)
 vim.keymap.set('n', 'gd', vim.lsp.buf.type_definition, opts)
 vim.keymap.set('n', '<space>k', vim.lsp.buf.hover, opts)
 vim.keymap.set('n', '<space>K', vim.lsp.buf.signature_help, opts)
 vim.keymap.set('n', '<space>i', vim.lsp.buf.implementation, opts)
 vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
 vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
 vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
 vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, opts)
 vim.keymap.set('n', '<space>rf', vim.lsp.buf.references, opts)
 vim.keymap.set('n', '<space>f', vim.lsp.buf.format, opts)
 vim.keymap.set('n', '<space>wl', function()
  print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, opts
 )
 end,
})

--vim.api.nvim_create_autocmd({'FileType'}, {
--  desc = 'Haskell lsp bindings',
--  pattern = 'haskell',
--  callback = function()
--    local ht = require('haskell-tools')
--    local bufnr = vim.api.nvim_get_current_buf()
--    local opts = { noremap = true, silent = true, buffer = bufnr, }
--    -- haskell-language-server relies heavily on codeLenses,
--    -- so auto-refresh (see advanced configuration) is enabled by default
--    vim.keymap.set('n', '<space>cl', vim.lsp.codelens.run, opts)
--    -- Hoogle search for the type signature of the definition under the cursor
--    vim.keymap.set('n', '<space>hs', ht.hoogle.hoogle_signature, opts)
--    -- Evaluate all code snippets
--    vim.keymap.set('n', '<space>ea', ht.lsp.buf_eval_all, opts)
--    -- Toggle a GHCi repl for the current package
--    vim.keymap.set('n', '<leader>rr', ht.repl.toggle, opts)
--    -- Toggle a GHCi repl for the current buffer
--    vim.keymap.set('n', '<leader>rf', function()
--      ht.repl.toggle(vim.api.nvim_buf_get_name(0))
--    end, opts)
--    vim.keymap.set('n', '<leader>rq', ht.repl.quit, opts)
--  end
--})

-- Visuals.
-- disable virtual_text (inline) diagnostics and use floating window, format the message such that it shows source, message and the error code. Show the message with <space>e
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
  format = function(diagnostic)
   return string.format(
    "%s (%s) [%s]",
    diagnostic.message,
    diagnostic.source,
    diagnostic.code or diagnostic.user_data.lsp.code
   )
  end,
 },
})


-- Transparent and groovy popup.
vim.api.nvim_set_hl(0, "Normal"                    , { bg = "none"    })
vim.api.nvim_set_hl(0, "NormalFloat"               , { bg = "none"    })
vim.api.nvim_set_hl(0, "FloatBorder"               , { fg = "#7d8618", bg = "none"})
vim.api.nvim_set_hl(0, "DiagnosticError"           , { fg = "#fb4934" })
vim.api.nvim_set_hl(0, "DiagnosticWarn"            , { fg = "#fe8019" })
vim.api.nvim_set_hl(0, "DiagnosticInfo"            , { fg = "#8ec07c" })
vim.api.nvim_set_hl(0, "DiagnosticHint"            , { fg = "#83a598" })
vim.api.nvim_set_hl(0, "DiagnosticOk"              , { fg = "#fabd2f" })
vim.api.nvim_set_hl(0, "DiagnosticVirtualTextError", { fg = "#fb4934" })
vim.api.nvim_set_hl(0, "DiagnosticVirtualTextWarn" , { fg = "#fe8019" })
vim.api.nvim_set_hl(0, "DiagnosticVirtualTextInfo" , { fg = "#8ec07c" })
vim.api.nvim_set_hl(0, "DiagnosticVirtualTextHint" , { fg = "#83a598" })
vim.api.nvim_set_hl(0, "DiagnosticVirtualTextOk"   , { fg = "#fabd2f" })
vim.api.nvim_set_hl(0, "DiagnosticUnderlineError"  , { fg = "#fb4934" })
vim.api.nvim_set_hl(0, "DiagnosticUnderlineWarn"   , { fg = "#fe8019" })
vim.api.nvim_set_hl(0, "DiagnosticUnderlineInfo"   , { fg = "#8ec07c" })
vim.api.nvim_set_hl(0, "DiagnosticUnderlineHint"   , { fg = "#83a598" })
vim.api.nvim_set_hl(0, "DiagnosticUnderlineOk"     , { fg = "#fabd2f" })
vim.api.nvim_set_hl(0, "DiagnosticFloatingError"   , { fg = "#fb4934" })
vim.api.nvim_set_hl(0, "DiagnosticFloatingWarn"    , { fg = "#fe8019" })
vim.api.nvim_set_hl(0, "DiagnosticFloatingInfo"    , { fg = "#8ec07c" })
vim.api.nvim_set_hl(0, "DiagnosticFloatingHint"    , { fg = "#83a598" })
vim.api.nvim_set_hl(0, "DiagnosticFloatingOk"      , { fg = "#fabd2f" })
vim.api.nvim_set_hl(0, "DiagnosticSignError"       , { fg = "#fb4934" })
vim.api.nvim_set_hl(0, "DiagnosticSignWarn"        , { fg = "#fe8019" })
vim.api.nvim_set_hl(0, "DiagnosticSignInfo"        , { fg = "#8ec07c" })
vim.api.nvim_set_hl(0, "DiagnosticSignHint"        , { fg = "#83a598" })
vim.api.nvim_set_hl(0, "DiagnosticSignOk"          , { fg = "#fabd2f" })
vim.api.nvim_set_hl(0, "DiagnosticDeprecated"      , { fg = "#d3869b" })
vim.api.nvim_set_hl(0, "DiagnosticUnnecessary"     , { fg = "#d3869b" })
-- Global mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist)

local null_ls = require("null-ls")
null_ls.setup({
 sources = {
  null_ls.builtins.code_actions.proselint,
  null_ls.builtins.formatting.shfmt,
  null_ls.builtins.formatting.stylua,
  null_ls.builtins.diagnostics.cppcheck,
  null_ls.builtins.formatting.isort,
  null_ls.builtins.formatting.yapf.with({
  extra_args = { "--style", vim.fn.expand("$XDG_CONFIG_HOME/yapf/yapf.toml")}
  }),
 },
 debug = false,
})
EOF

" Treesitter
lua << EOF
require'nvim-treesitter.configs'.setup {
 -- A list of parser names, or "all" (the five listed parsers should always be installed)
 -- ensure_installed = { "c", "lua", "vim", "vimdoc", "query", "all" },
 ensure_installed = { all },
 -- Install parsers synchronously (only applied to `ensure_installed`)
 sync_install = false,
 -- Automatically install missing parsers when entering buffer
 -- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
 auto_install = true,
 -- List of parsers to ignore installing (or "all")
 ignore_install = { markdown },
 ---- If you need to change the installation directory of the parsers (see -> Advanced Setup)
 -- parser_install_dir = "/some/path/to/store/parsers", -- Remember to run vim.opt.runtimepath:append("/some/path/to/store/parsers")!
 highlight = {
  enable = true,
  -- NOTE: these are the names of the parsers and not the filetype. (for example if you want to
  -- disable highlighting for the `tex` filetype, you need to include `latex` in this list as this is
  -- the name of the parser)
  -- list of language that will be disabled
  disable = { markdown },
  -- Or use a function for more flexibility, e.g. to disable slow treesitter highlight for large files
  disable = function(lang, buf)
  local max_filesize = 100 * 1024 -- 100 KB
  local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
  if ok and stats and stats.size > max_filesize then
   return true
   end
   end,
   -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
   -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
   -- Using this option may slow down your editor, and you may see some duplicate highlights.
   -- Instead of true it can also be a list of languages
   additional_vim_regex_highlighting = false,
 },
 indent = {
  enable = true,
 },
}
EOF
autocmd VimEnter * TSEnable highlight
autocmd VimEnter * TSEnable indent
autocmd VimEnter * TSEnable incremental_selection

" Wrapper.nvim plug.
"lua << EOF
" require("wrapping").setup()
"EOF

" Automaton plug.
nmap <leader>fml <cmd>CellularAutomaton make_it_rain<CR>

" Rainbow parentheses.
lua << EOF
 -- This module contains a number of default definitions
 local rainbow_delimiters = require 'rainbow-delimiters'
 vim.g.rainbow_delimiters = {
  strategy = {
   [''] = rainbow_delimiters.strategy['global'],
   vim = rainbow_delimiters.strategy['global'],
  },
  query = {
   [''] = 'rainbow-delimiters',
   lua = 'rainbow-delimiters',
  },
  priority = {
   [''] = 110,
   lua = 210,
  },
  highlight = {
   'RainbowDelimiterRed',
   'RainbowDelimiterYellow',
   'RainbowDelimiterBlue',
   'RainbowDelimiterOrange',
   'RainbowDelimiterGreen',
   'RainbowDelimiterViolet',
   'RainbowDelimiterCyan',
  },
 }
 query = {
  -- Use parentheses by default
  [''] = 'rainbow-delimiters',
  -- Use blocks for Lua
  lua = 'rainbow-delimiters',
  -- Determine the query dynamically
  query = function(bufnr)
  -- Use blocks for read-only buffers like in `:InspectTree`
  local is_nofile = vim.bo[bufnr].buftype == 'nofile'
  return is_nofile and 'rainbow-blocks' or 'rainbow-delimiters'
  end
 }
EOF

" Treesitter context.
lua << EOF
 require'treesitter-context'.setup{
  enable = true, -- Enable this plugin (Can be enabled/disabled later via commands)
  max_lines = 0, -- How many lines the window should span. Values <= 0 mean no limit.
  min_window_height = 30, -- Minimum editor window height to enable context. Values <= 0 mean no limit.
  line_numbers = true,
  multiline_threshold = 20, -- Maximum number of lines to show for a single context
  trim_scope = 'outer', -- Which context lines to discard if `max_lines` is exceeded. Choices: 'inner', 'outer'
  mode = 'cursor',  -- Line used to calculate context. Choices: 'cursor', 'topline'
  -- Separator between context and content. Should be a single character string, like '-'.
  -- When separator is set, the context will only show up when there are at least 2 lines above cursorline.
  separator = nil,
  zindex = 20, -- The Z-index of the context window
  on_attach = nil, -- (fun(buf: integer): boolean) return false to disable attaching
 }
 vim.keymap.set("n", "[c", function()
  require("treesitter-context").go_to_context(vim.v.count1)
 end, { silent = true })
EOF

" Telescope
lua << EOF
local Layout = require("nui.layout")
local Popup = require("nui.popup")
local telescope = require("telescope")
local TSLayout = require("telescope.pickers.layout")
local function make_popup(options)
  local popup = Popup(options)
  function popup.border:change_title(title)
    popup.border.set_text(popup.border, "top", title)
  end
  return TSLayout.Window(popup)
end
telescope.setup({
  defaults = {
    layout_strategy = "flex",
    layout_config = {
      horizontal = {
        size = {
          width = "90%",
          height = "60%",
        },
      },
      vertical = {
        size = {
          width = "90%",
          height = "90%",
        },
      },
    },
    create_layout = function(picker)
      local border = {
        results = {
          top_left = "┌",
          top = "─",
          top_right = "┬",
          right = "│",
          bottom_right = "",
          bottom = "",
          bottom_left = "",
          left = "│",
        },
        results_patch = {
          minimal = {
            top_left = "┌",
            top_right = "┐",
          },
          horizontal = {
            top_left = "┌",
            top_right = "┬",
          },
          vertical = {
            top_left = "├",
            top_right = "┤",
          },
        },
        prompt = {
          top_left = "├",
          top = "─",
          top_right = "┤",
          right = "│",
          bottom_right = "┘",
          bottom = "─",
          bottom_left = "└",
          left = "│",
        },
        prompt_patch = {
          minimal = {
            bottom_right = "┘",
          },
          horizontal = {
            bottom_right = "┴",
          },
          vertical = {
            bottom_right = "┘",
          },
        },
        preview = {
          top_left = "┌",
          top = "─",
          top_right = "┐",
          right = "│",
          bottom_right = "┘",
          bottom = "─",
          bottom_left = "└",
          left = "│",
        },
        preview_patch = {
          minimal = {},
          horizontal = {
            bottom = "─",
            bottom_left = "",
            bottom_right = "┘",
            left = "",
            top_left = "",
          },
          vertical = {
            bottom = "",
            bottom_left = "",
            bottom_right = "",
            left = "│",
            top_left = "┌",
          },
        },
      }

      local results = make_popup({
        focusable = false,
        border = {
          style = border.results,
          text = {
            top = picker.results_title,
            top_align = "center",
          },
        },
        win_options = {
          winhighlight = "Normal:Normal",
        },
      })

      local prompt = make_popup({
        enter = true,
        border = {
          style = border.prompt,
          text = {
            top = picker.prompt_title,
            top_align = "center",
          },
        },
        win_options = {
          winhighlight = "Normal:Normal",
        },
      })

      local preview = make_popup({
        focusable = false,
        border = {
          style = border.preview,
          text = {
            top = picker.preview_title,
            top_align = "center",
          },
        },
      })

      local box_by_kind = {
        vertical = Layout.Box({
          Layout.Box(preview, { grow = 1 }),
          Layout.Box(results, { grow = 1 }),
          Layout.Box(prompt, { size = 3 }),
        }, { dir = "col" }),
        horizontal = Layout.Box({
          Layout.Box({
            Layout.Box(results, { grow = 1 }),
            Layout.Box(prompt, { size = 3 }),
          }, { dir = "col", size = "50%" }),
          Layout.Box(preview, { size = "50%" }),
        }, { dir = "row" }),
        minimal = Layout.Box({
          Layout.Box(results, { grow = 1 }),
          Layout.Box(prompt, { size = 3 }),
        }, { dir = "col" }),
      }

      local function get_box()
        local strategy = picker.layout_strategy
        if strategy == "vertical" or strategy == "horizontal" then
          return box_by_kind[strategy], strategy
        end

        local height, width = vim.o.lines, vim.o.columns
        local box_kind = "horizontal"
        if width < 100 then
          box_kind = "vertical"
          if height < 40 then
            box_kind = "minimal"
          end
        end
        return box_by_kind[box_kind], box_kind
      end

      local function prepare_layout_parts(layout, box_type)
        layout.results = results
        results.border:set_style(border.results_patch[box_type])

        layout.prompt = prompt
        prompt.border:set_style(border.prompt_patch[box_type])

        if box_type == "minimal" then
          layout.preview = nil
        else
          layout.preview = preview
          preview.border:set_style(border.preview_patch[box_type])
        end
      end

      local function get_layout_size(box_kind)
        return picker.layout_config[box_kind == "minimal" and "vertical" or box_kind].size
      end

      local box, box_kind = get_box()
      local layout = Layout({
        relative = "editor",
        position = "50%",
        size = get_layout_size(box_kind),
      }, box)

      layout.picker = picker
      prepare_layout_parts(layout, box_kind)

      local layout_update = layout.update
      function layout:update()
        local box, box_kind = get_box()
        prepare_layout_parts(layout, box_kind)
        layout_update(self, { size = get_layout_size(box_kind) }, box)
      end

      return TSLayout(layout)
    end,
  },
 pickers = {
  find_files = {
   no_ignore = true,
  },
 },
})
 require('telescope').load_extension('fzf')
 local builtin = require('telescope.builtin')
 vim.keymap.set('n', '<leader>tf', builtin.find_files, {})
 vim.keymap.set('n', '<leader>tg', builtin.live_grep, {})
 vim.keymap.set('n', '<leader>tb', builtin.buffers, {})
 vim.keymap.set('n', '<leader>th', builtin.help_tags, {})
EOF


" UFO folds
lua << EOF
vim.o.foldcolumn = '0' -- '0' is not bad
vim.o.foldlevel = 400 -- Using ufo provider need a large value, feel free to decrease the value
vim.o.foldlevelstart = 400
vim.o.foldenable = true
-- Using ufo provider need remap `zR` and `zM`. If Neovim is 0.6.1, remap yourself
vim.keymap.set('n', 'zR', require('ufo').openAllFolds)
vim.keymap.set('n', 'zM', require('ufo').closeAllFolds)
vim.keymap.set('n', 'zr', require('ufo').openFoldsExceptKinds)
vim.keymap.set('n', 'zm', require('ufo').closeFoldsWith) -- closeAllFolds == closeFoldsWith(0)
local handler = function(virtText, lnum, endLnum, width, truncate)
    local newVirtText = {}
    local suffix = (' %dΩ'):format(endLnum - lnum)
    local sufWidth = vim.fn.strdisplaywidth(suffix)
    local targetWidth = width - sufWidth
    local curWidth = 0
    for _, chunk in ipairs(virtText) do
        local chunkText = chunk[1]
        local chunkWidth = vim.fn.strdisplaywidth(chunkText)
        if targetWidth > curWidth + chunkWidth then
            table.insert(newVirtText, chunk)
        else
            chunkText = truncate(chunkText, targetWidth - curWidth)
            local hlGroup = chunk[2]
            table.insert(newVirtText, {chunkText, hlGroup})
            chunkWidth = vim.fn.strdisplaywidth(chunkText)
            -- str width returned from truncate() may less than 2nd argument, need padding
            if curWidth + chunkWidth < targetWidth then
                suffix = suffix .. (' '):rep(targetWidth - curWidth - chunkWidth)
            end
            break
        end
        curWidth = curWidth + chunkWidth
    end
    table.insert(newVirtText, {suffix, 'MoreMsg'})
    return newVirtText
end
-- Treesitter as a main provider
require('ufo').setup({
    provider_selector = function(bufnr, filetype, buftype)
        return {'treesitter', 'indent'}
    end,
    fold_virt_text_handler = handler
})
EOF
" Leap.nvim leap
lua << EOF
require('leap').create_default_mappings()
EOF

" Which key which-key.
lua << EOF
local status_ok, which_key = pcall(require, "which-key")
if not status_ok then
    return
end
local setup = {
    plugins = {
        marks = true, -- shows a list of your marks on ' and `
        registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
        spelling = {
            enabled = true, -- enabling this will show WhichKey when pressing z= to select spelling suggestions
            suggestions = 20, -- how many suggestions should be shown in the list?
        },
        -- the presets plugin, adds help for a bunch of default keybindings in Neovim
        -- No actual key bindings are created
        presets = {
            operators = false, -- adds help for operators like d, y, ... and registers them for motion / text object completion
            motions = true, -- adds help for motions
            text_objects = true, -- help for text objects triggered after entering an operator
            windows = true, -- default bindings on <c-w>
            nav = true, -- misc bindings to work with windows
            z = true, -- bindings for folds, spelling and others prefixed with z
            g = true, -- bindings for prefixed with g
        },
    },
    -- add operators that will trigger motion and text object completion
    -- to enable all native operators, set the preset / operators plugin above
    -- operators = { gc = "Comments" },
    key_labels = {
        -- override the label used to display some keys. It doesn't effect WK in any other way.
        -- For example:
        -- ["<space>"] = "SPC",
        -- ["<cr>"] = "RET",
        -- ["<tab>"] = "TAB",
    },
    icons = {
        breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
        separator = "➜", -- symbol used between a key and it's label
        group = "+", -- symbol prepended to a group
    },
    popup_mappings = {
        scroll_down = "<c-d>", -- binding to scroll down inside the popup
        scroll_up = "<c-u>", -- binding to scroll up inside the popup
    },
    window = {
        border = "rounded", -- none, single, double, shadow
        position = "bottom", -- bottom, top
        margin = { 1, 0, 1, 0 }, -- extra window margin [top, right, bottom, left]
        padding = { 2, 2, 2, 2 }, -- extra window padding [top, right, bottom, left]
        winblend = 0,
    },
    layout = {
        height = { min = 4, max = 25 }, -- min and max height of the columns
        width = { min = 20, max = 50 }, -- min and max width of the columns
        spacing = 3, -- spacing between columns
        align = "left", -- align columns left, center or right
    },
    ignore_missing = true, -- enable this to hide mappings for which you didn't specify a label
    hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ " }, -- hide mapping boilerplate
    show_help = true, -- show help message on the command line when the popup is visible
    triggers = "auto", -- automatically setup triggers
    -- triggers = {"<leader>"} -- or specify a list manually
    triggers_blacklist = {
        -- list of mode / prefixes that should never be hooked by WhichKey
        -- this is mostly relevant for key maps that start with a native binding
        -- most people should not need to change this
        i = { "j", "k" },
        v = { "j", "k" },
    },
}
local opts = {
    mode = "n", -- NORMAL mode
    prefix = "<leader>",
    buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
    silent = true, -- use `silent` when creating keymaps
    noremap = true, -- use `noremap` when creating keymaps
    nowait = true, -- use `nowait` when creating keymaps
}
local mappings = {

    ["k"] = { "<cmd>bdelete<CR>", "Kill Buffer" },  -- Close current file
    ["q"] = { "<cmd>wqall!<CR>", "Quit" }, -- Quit Neovim after saving the file
    ["w"] = { "<cmd>w!<CR>", "Save" }, -- Save current file
}
which_key.setup(setup)
which_key.register(mappings, opts)
EOF

" Nerdtree (file tree).
"nnoremap <leader>nf :NERDTreeFocus<CR>
"nnoremap <leader>n :NERDTree<CR>
nnoremap <leader>nt :NERDTreeToggle<CR>
nnoremap <leader>ns :NERDTreeFind<CR>
nnoremap <silent> <C-o> :<CR>

" Open/close quickfix on toggle
function! ToggleQuickFix()
    if empty(filter(getwininfo(), 'v:val.quickfix'))
        copen
    else
        cclose
    endif
endfunction

nnoremap <silent> F :call ToggleQuickFix()<cr>

"tabfix
set smarttab
set autoindent
set expandtab

lua << EOF
local cmp = {} -- statusline components

--- highlight pattern
-- This has three parts: 
-- 1. the highlight group
-- 2. text content
-- 3. special sequence to restore highlight: %*
-- Example pattern: %#SomeHighlight#some-text%*
local hi_pattern = '%%#%s#%s%%*'

function _G._statusline_component(name)
  return cmp[name]()
end

function cmp.diagnostic_status()
  local ok = ''

  local ignore = {
    ['c'] = true, -- command mode
    ['t'] = true  -- terminal mode
  }

  local mode = vim.api.nvim_get_mode().mode

  if ignore[mode] then
    return ok
  end

  local levels = vim.diagnostic.severity
  local errors = #vim.diagnostic.get(0, {severity = levels.ERROR})
  if errors > 0 then
    return 'ERROR '
  end

  local warnings = #vim.diagnostic.get(0, {severity = levels.WARN})
  if warnings > 0 then
    return 'WARN '
  end

  return ok
end


local statusline = {
  '%{%v:lua._statusline_component("diagnostic_status")%}',
  '%t',
  '%r',
  '%m',
  '%=',
  '%{&filetype} ',
  '%2p%%',
}

vim.o.statusline = table.concat(statusline, '')

vim.o.statusline = table.concat(statusline, '')
EOF
" Move around recently opened files
nmap <silent> <leader><lt> ;bnext<cr> 
nmap <silent> <leader>> ;bprevious<cr> 
