if exists('g:loaded_scalacompletion')
  finish
endif

let g:loaded_scalacompletion = 1
let g:started_scalacompletion = 0
let g:scalacompletion_error = 0

fu! scalacompletion#Complete(findstart, base)
  if a:findstart == 1
    return s:startOfWord()
  else
    return s:doCompletion(a:base)
  endif
endf

fu! scalacompletion#Start()
  echom "Starting scala completion server"

  let path = s:absolutePath()
  let server_url = "http://localhost:8085/"
  let command = 'curl -s --data "file_path='.s:urlEncode(path).'" "'.server_url.'init"'
  let response = system(command)
  let response_str = eval('"'.response.'"')

  echom response_str

  let g:started_scalacompletion = 1
endfu

fu! s:startOfWord()
  let line = getline('.')
  let start = col('.') - 1
  " TODO: regex that looks more like scala identifier
  " while start > 0 && line[start - 1] =~ '\a'
  while start > 0 && line[start - 1] =~ '[a-zA-Zа-яА-Я]'
    let start -= 1
  endwhile
  return start
endfu

fu! s:doCompletion(prefix)
  if !g:started_scalacompletion && !g:scalacompletion_error
    call scalacompletion#Start()
  endif

  let lineIdx = line('.') - 1
  let columnIdx = virtcol('.') - 1
  let name = s:absolutePath()
  let tmpFilePath = s:saveCurrentBufferToTempFile()

  let server_url = "http://localhost:8085/"
  let command = 'curl -s "'.server_url.'completion?name='.s:urlEncode(name).'&file_path='.s:urlEncode(tmpFilePath).'&line='.lineIdx.'&column='.columnIdx
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
    let g:scalacompletion_error = 1
    echom 'Completion failed. Response from server: '.result
    return []
  endtry
endfu

fu! s:absolutePath()
  return expand("%:p")
endfu

fu! s:saveCurrentBufferToTempFile()
  let buf = getline(1, '$')
  " if &l:fileformat == 'dos'
  "   " XXX: line2byte() depend on 'fileformat' option.
  "   " so if fileformat is 'dos', 'buf' must include '\r'.
  "   let buf = map(buf, 'v:val."\r"')
  " endif
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

fu! scalacompletion#InsertPkgName()
  let name = s:absolutePath()
  let server_url = "http://localhost:8085/"
  let command = 'curl -s "'.server_url.'package?file_path='.s:urlEncode(name).'"'
  let result = system(command)
  call append(0, 'package '.result)
endf

fu! scalacompletion#AddImport()
  let wordUnderCursor = expand('<cword>')
  let name = s:absolutePath()

  let server_url = "http://localhost:8085/"
  let command = 'curl -s "'.server_url.'imports?file_path='.s:urlEncode(name).'&class_name='.wordUnderCursor.'"'
  let result = system(command)
  let suggestions = eval(result)

  if empty(suggestions)
    echohl Error | echo "\nNo candidates found for " . wordUnderCursor | echohl None
    return ''
  endif

  " construct menu
  let suggestions_menu_list = ['Select class to import:']
  for i in range(0, len(suggestions) - 1)
    call add(suggestions_menu_list, (i+1) . ") " . suggestions[i])
  endfor

  " prompt for class
  let idx = inputlist(suggestions_menu_list)
  " cancelled
  if idx <= 0
    return ''
  endif

  " to list index
  let idx = idx - 1
  if idx >= len(suggestions)
    echohl Error | echo "\nIncorrect selection" | echohl None
    return ''
  endif

  let import_statement = 'import' . " " . suggestions[idx]

  " search from beginnig of file
  call cursor(1, 1)
  if searchpos("^" . import_statement . ";?$", 'nW')[0] != 0
    echohl Error | echo "\nThis class is already imported" | echohl None
    return ''
  endif
  " append to the beginning of file if nothing else is found
  let append_import_to = 0
  " jump to the last line to start searching backwards
  call cursor(line('$'), 1)
  " search for the last import statement
  let last_import_pos = searchpos('^import', 'bW')[0]
  if last_import_pos != 0
    let append_import_to = last_import_pos
  else
    " if imports not found, try to find package declaration
    let package_pos = searchpos('^package')[0]
    if package_pos != 0
      let append_import_to = package_pos
    endif
  endif

  call append(append_import_to, import_statement)

  return ''
endfu
