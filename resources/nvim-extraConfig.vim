" TODO put this shit in configuration.nix
lua << EOF
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

-- Make lsp popups pretty
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

vim.api.nvim_create_autocmd({'FileType'}, {
  desc = 'keymap \'q\' to close help/quickfix/netrw/etc windows',
  pattern = 'help,qf,netrw',
  callback = function()
   vim.keymap.set('n', 'Q', '<C-w>c', {buffer = true, desc = 'Quit (or Close) help, quickfix, netrw, etc windows', })
  end
})

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

vim.api.nvim_set_hl(0, "FloatBorder"               , { fg = "#7d8618", bg = "none"})
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

" Perform dot commands over visual blocks
vnoremap . :normal .<CR>
