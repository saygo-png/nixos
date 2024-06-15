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
EOF

augroup remember_folds
  autocmd!
  au BufWinLeave ?* mkview 1
  au BufWinEnter ?* silent! loadview 1
augroup END

hi statusline ctermbg=NONE guibg=NONE gui=none guifg=#7d8618
hi LineNr guifg=#7d8618
hi noCursor blend=100 cterm=strikethrough
hi ModeMsg guifg=#7d8618
hi MsgArea guifg=#7d8618

