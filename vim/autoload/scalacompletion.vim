" if exists('g:loaded_scalacompletion')
"   finish
" endif

" let g:loaded_scalacompletion = 1

fu! scalacompletion#Complete(findstart, base)
  if a:findstart == 1
    return s:startOfWord()
  else
    return s:doCompletion(a:base)
  endif
endf

fu! scalacompletion#Start()
  let config_file_name = 'vim_scala_completion.conf'
  let project_root = scalacompletion#FindProjectRoot(config_file_name)
  echom project_root
  if project_root == '0'
    echoerr "Can't find project configured for vim-scala-completion! ".config_file_name. " file required in project root."
    return
  endif

  let config_path = project_root.'/'.config_file_name
  let server_url = "http://localhost:8085/"
  let command = 'curl -s --data "conf='.s:urlEncode(config_path).'" "'.server_url.'init"'
  let response = system(command)
  let response_str = eval('"'.response.'"')

  if response_str != config_path
    echoerr "Unexpected response from server: ".response_str
    return
  endif
  echom "Project started successfuly"
endfu

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
  let name = s:absolutePath()
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

fu! s:absolutePath()
  return expand("%:p")
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

fu! scalacompletion#FindProjectRoot(lookFor)
  let pathMaker = '%:p'
  let pathMaker=pathMaker.':h'
  let fileToCheck = expand(pathMaker).'/'.a:lookFor
  if filereadable(fileToCheck)
    return expand(pathMaker)
  endif
  while (len(expand(pathMaker)) > len(expand(pathMaker.':h')))
    let pathMaker=pathMaker.':h'
    let fileToCheck = expand(pathMaker).'/'.a:lookFor
    if filereadable(fileToCheck)
      return expand(pathMaker)
    endif
  endwhile
  return 0
endf
