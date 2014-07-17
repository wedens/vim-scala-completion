" if exists('g:loaded_scalacompletion')
"   finish
" endif

" let g:loaded_scalacompletion = 1

fu! scalacompletion#Complete(findstart, base)
  if a:findstart == 1 "findstart = 1 when we need to get the text length
    return s:startOfWord()
  else "findstart = 0 when we need to return the list of completions
    return s:doCompletion(a:base)
  endif
endf

fu! s:startOfWord()
  let line = getline('.')
  let start = col('.') - 1
  while start > 0 && line[start - 1] =~ '\a'
    let start -= 1
  endwhile
  return start
endfu

fu! s:doCompletion(prefix)
  let offset = s:cursorOffset()
  let name = s:relativePath()
  let tmpFilePath = s:saveCurrentBufferToTempFile()
  let column = s:startOfWord() - 1

  let server_url = "http://localhost:8085/"
  let command = 'curl -s "'.server_url.'completion?name='.s:urlEncode(name).'&file_path='.s:urlEncode(tmpFilePath).'&offset='.offset.'&column='.column
  if len(a:prefix) > 0
    let command = command."&prefix=".s:urlEncode(a:prefix)
  endif
  let command = command.'"'

  let result = system(command)

  call delete(tmpFilePath)

  try
    let completions = eval(result)
    return completions
  catch
    echom 'Completion failed. Response from server: '.result
    return []
  endtry
endfu

fu! s:cursorOffset()
  return printf('%d', line2byte(line('.')) + (col('.')-2))
endfu

fu! s:relativePath()
  return expand("%")
endfu

fu! s:saveCurrentBufferToTempFile()
  let buf = getline(1, '$')
  if &l:fileformat == 'dos'
    " XXX: line2byte() depend on 'fileformat' option.
    " so if fileformat is 'dos', 'buf' must include '\r'.
    let buf = map(buf, 'v:val."\r"')
  endif
  let file = tempname()
  call writefile(buf, file)
  return file
endf

fu! s:encodeChar(char)
    if a:char == '%'
        return '%%'
    elseif a:char == ' '
        return '+'
    else
        return printf("%%%X", char2nr(a:char))
    endif
endf

fu! s:urlEncode(str)
    let rx = '\\([^a-zA-Z0-9_.-]\\)'
    let rv = substitute(a:str, rx, '\\=s:encodeChar(submatch(1))', 'g')
    return rv
endf
