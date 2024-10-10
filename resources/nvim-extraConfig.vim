" Ugliest piece of shit code because i wrote it
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
