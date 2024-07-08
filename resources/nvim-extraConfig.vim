" TODO put this shit in configuration.nix
" Set contrast.
" This configuration option should be placed before `colorscheme gruvbox-material`.
" Available values: 'hard', 'medium'(default), 'soft'

" Colorscheme.
if has('termguicolors')
  set termguicolors
endif

lua << EOF

-- Gray out leap
vim.api.nvim_set_hl(0, 'LeapBackdrop', { link = 'Comment' })

-- Leap bidirectional search
vim.keymap.set('n',        's', '<Plug>(leap)')
vim.keymap.set('n',        'S', '<Plug>(leap-from-window)')
vim.keymap.set({'x', 'o'}, 's', '<Plug>(leap-forward)')
vim.keymap.set({'x', 'o'}, 'S', '<Plug>(leap-backward)')

-- Statusline
local cmp = {} -- statusline components
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

function cmp.git_status()
  local git_info = vim.b.gitsigns_status_dict
  if not git_info or git_info.head == "" then
    return ""
  end
  local added = git_info.added and ("%#GitSignsAdd#+" .. git_info.added .. " ") or ""
  local changed = git_info.changed and ("%#GitSignsChange#~" .. git_info.changed .. " ") or ""
  local removed = git_info.removed and ("%#GitSignsDelete#-" .. git_info.removed .. " ") or ""
  if git_info.added == 0 then
    added = ""
  end
  if git_info.changed == 0 then
    changed = ""
  end
  if git_info.removed == 0 then
    removed = ""
  end
  return table.concat {
     " ",
     added,
     changed,
     removed,
     "%#GitSignsAdd#branch ",
     git_info.head,
     " %#Normal#",
  }
end

function cmp.lint_progress()
  local linters = require("lint").get_running()
  if #linters == 0 then
      return "lint: ok"
  end
  return "lint: search" .. table.concat(linters, ", ")
end

local statusline = {
  '%{%v:lua._statusline_component("diagnostic_status")%}',
  '%t',
  '%r',
  '%m',
  '%{%v:lua._statusline_component("git_status")%}',
  '%{%v:lua._statusline_component("lint_progress")%}',
  '%=',
  '%{&filetype} ',
  '%2p%%',
}

vim.o.statusline = table.concat(statusline, '')

-- vim.api.nvim_create_autocmd({'FileType'}, {
--   desc = 'keymap \'q\' to close help/quickfix/netrw/etc windows',
--   pattern = 'help,qf,netrw',
--   callback = function()
--    vim.keymap.set('n', 'Q', '<C-w>c', {buffer = true, desc = 'Quit (or Close) help, quickfix, netrw, etc windows', })
--   end
-- })
-- Close buffer
-- vim.keymap.set("n", "Q", ":close<CR>", { desc = "Close the current buffer" })

-- Keep selection when indenting.
vim.keymap.set("v", ">", ">gv", { desc = "Keep selection after indenting" })
vim.keymap.set("v", "<", "<gv", { desc = "Keep selection after unindenting" })

-- Window switching.
vim.keymap.set("n", "<C-h>", ":wincmd h<CR>", { desc = "Move to the split on the left side" })
vim.keymap.set("n", "<C-l>", ":wincmd l<CR>", { desc = "Move to the split on the right side" })
vim.keymap.set("n", "<C-k>", ":wincmd k<CR>", { desc = "Move to the split above" })
vim.keymap.set("n", "<C-j>", ":wincmd j<CR>", { desc = "Move to the split below" })
EOF

augroup remember_folds
  autocmd!
  au BufWinLeave ?* mkview 1
  au BufWinEnter ?* silent! loadview 1
augroup END

" Hide cursorline when unfocused.
let my_cursor_style = &guicursor
augroup cursorline
  autocmd!
  autocmd FocusGained,WinEnter * let &guicursor = my_cursor_style
  autocmd FocusGained,WinEnter * setlocal cursorline
  autocmd FocusLost,WinLeave * setlocal nocursorline guicursor=a:noCursor/lCursor
augroup END

" Open url in visual selection.
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
    let column_end -= 1 "Needed to remove the last character to make it match the visual selection
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
   return join(lines)  "returns selection as a string of space separated line
  endfunction
vnoremap gx :<BS><BS><BS><BS><BS>execute '!openlisturl' shellescape(GetVisualSelection())<CR>

" Infinite paste
vnoremap <expr> p 'pgvy'

" Blank or empty line jump
noremap { <Cmd>call search('^\s*\S', 'Wbc') \| call search('^\s*$\\|\%^', 'Wb')<CR>
noremap } <Cmd>call search('^\s*\S', 'Wc') \| call search('^\s*$\\|\%$', 'W')<CR>

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

" Makes ctrl+s increment to not conflict with tmux.
nnoremap <C-s> <C-a>

" Center search and substitution.
nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zzo

" Open/close quickfix on toggle
function! ToggleQuickFix()
    if empty(filter(getwininfo(), 'v:val.quickfix'))
        copen
    else
        cclose
    endif
endfunction

nnoremap <silent> F :call ToggleQuickFix()<cr>

" Faster syntax highlight.
syntax sync minlines=256
