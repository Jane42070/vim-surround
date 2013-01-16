" surround.vim - Surroundings
" Author:       Tim Pope <vimNOSPAM@tpope.org>
" Version:      1.90
" GetLatestVimScripts: 1697 1 :AutoInstall: surround.vim

if exists("g:loaded_surround") || v:version < 700
  finish
endif
let g:loaded_surround = 1

let s:cpo_save = &cpo
set cpo&vim

" Surround object {{{1

function! s:get_surround_object(seq)
  let null_sobj = {'left': '', 'right': '', 'nspaces': 0, 'reindent': 0}

  let mlist = matchlist(a:seq, "\\v^( *)([\<C-d>\<C-t>]?)(.*)$")
  let extraspaces = len(mlist[1])
  let reindent = mlist[2]
  let char = mlist[3]

  if exists("b:surround_objects") && has_key(b:surround_objects, char)
    let raw_sobj = b:surround_objects[char]
  elseif has_key(g:surround_objects, char)
    let raw_sobj = g:surround_objects[char]
  else
    if char !~ '\a'
      let raw_sobj = char . "\r" . char
    else
      return null_sobj
    endif
  endif

  if type(raw_sobj) == type({})
    let sobj = copy(raw_sobj)
  elseif type(raw_sobj) == type('')
    " Backward compatibility
    let raw_sobj = s:process(raw_sobj)
    let sobj = {'left': s:extractbefore(raw_sobj),
    \           'right': s:extractafter(raw_sobj),
    \           'nspaces': 0, 'reindent': 1}
  else
    return null_sobj
  endif

  let sobj['nspaces'] += extraspaces
  if reindent == "\<C-t>"
    let sobj['reindent'] = 1
  elseif reindent == "\<C-d>"
    let sobj['reindent'] = 0
  endif

  let Func = get(sobj, 'inputfunc', 0)
  if type(Func) == type(function('tr'))
    let values = call(Func, get(sobj, 'inputfuncargs', []))

    for i in range(len(values))
      let sobj['left']  = substitute(sobj['left'],  nr2char(i+1), values[i],'g')
      let sobj['right'] = substitute(sobj['right'], nr2char(i+1), values[i],'g')
    endfor
  endif

  return sobj
endfunction

" }}}1

" Input functions {{{1

function! s:getchar()
  let c = getchar()
  if c =~ '^\d\+$'
    let c = nr2char(c)
  endif
  return c
endfunction

function! s:inputtarget()
  let builtins = "wWsp[]()b<>t{}B\"'`"

  let cnt = ''
  let space = ''

  let c = s:getchar()
  while c =~ '\d'
    let cnt .= c
    let c = s:getchar()
  endwhile

  if c == " "
    let space = ' '
    let c = s:getchar()
  endif

  if c == "\<Esc>" || c == "\<C-c>"
    return ""
  endif

  let target = c

  if strlen(target) == 1 && stridx(builtins, target) != -1 &&
  \  mapcheck('a' . target, 'o') == '' && mapcheck('i' . target, 'o') == ''
    return cnt . space . target
  endif

  while mapcheck('a' . target, 'o') != '' && maparg('a' . target, 'o') == '' &&
  \     mapcheck('a' . target, 'o') != '' && maparg('i' . target, 'o') == ''
    let c  = s:getchar()
    if c  == "\<Esc>" || c == "\<C-c>"
      return ""
    endif
    let target .= c
  endwhile

  return cnt . space . target
endfunction

function! s:inputreplacement()
  let extraspaces = ''
  let reindent = ''

  while 1
    let c = s:getchar()
    if c == ' '
      let extraspaces .= c
    elseif c == "\<C-d>" || c == "\<C-t>"
      let reindent = c
    elseif c == "\<Esc>" || c == "\<C-c>"
      return ""
    else
      break
    endif
  endwhile

  let replacement = c
  let list = keys(g:surround_objects)

  while 1
    let list = filter(list, "v:val =~# '^\\V' . replacement")
    if len(list) == 0 || (len(list) == 1 && list[0] == replacement)
      break
    endif
    let c = s:getchar()
    if c == "\<Esc>" || c == "\<C-c>"
      return ""
    endif
    let replacement .= c
  endwhile

  return extraspaces . reindent . replacement
endfunction

function! s:simple_input(...)
  let args = a:0 == 0 ? ['input: '] : a:000
  let values = []

  for i in args
    call add(values, input(i))
  endfor

  return values
endfunction

function! s:tag_input()
  let values = s:simple_input('tag: ')
  call add(values, substitute(values[0], '\s.*', '', ''))
  return values
endfunction

function! s:tag_input_lastdel()
  let last = exists("s:lastdel") ? s:lastdel : ''
  let default = matchstr(last, '<\zs.\{-\}\ze>')
  let values = [input('tag: ', default)]
  call add(values, substitute(values[0], '\s.*', '', ''))
  return values
endfunction

function! s:latex_input()
  let pairs = "[](){}<>"
  let env = input('\begin{')
  let env = '{' . env
  let tail = matchstr(env, '\v.[^\[\]()){}}<>]+$')
  if tail != ""
    let env .= pairs[stridx(pairs, tail[0]) + 1]
  endif
  return [env, substitute(env, '[^}]*$', '', '')]
endfunction

" }}}1

" Wrapping functions {{{1

" Functions for backward compatibility {{{2

function! s:extractbefore(str)
  if a:str =~ '\r'
    return matchstr(a:str,'.*\ze\r')
  else
    return matchstr(a:str,'.*\ze\n')
  endif
endfunction

function! s:extractafter(str)
  if a:str =~ '\r'
    return matchstr(a:str,'\r\zs.*')
  else
    return matchstr(a:str,'\n\zs.*')
  endif
endfunction

function! s:process(string)
  let i = 0
  while i < 7
    let i = i + 1
    let repl_{i} = ''
    let m = matchstr(a:string,nr2char(i).'.\{-\}\ze'.nr2char(i))
    if m == ''
      continue
    elseif m[1] == "\e"
      let m = substitute(strpart(m,2),'\r.*','','')
      let repl_{i} = eval(m)
    else
      let m = substitute(strpart(m,1),'\r.*','','')
      let repl_{i} = input(substitute(m,':\s*$','','').': ')
    endif
  endwhile
  let s = ""
  let i = 0
  while i < strlen(a:string)
    let char = strpart(a:string,i,1)
    if char2nr(char) < 8
      let next = stridx(a:string,char,i+1)
      if next == -1
        let s = s . char
      else
        let insertion = repl_{char2nr(char)}
        let subs = strpart(a:string,i+1,next-i-1)
        let subs = matchstr(subs,'\r.*')
        while subs =~ '^\r.*\r'
          let sub = matchstr(subs,"^\r\\zs[^\r]*\r[^\r]*")
          let subs = strpart(subs,strlen(sub)+1)
          let r = stridx(sub,"\r")
          let insertion = substitute(insertion,strpart(sub,0,r),strpart(sub,r+1),'')
        endwhile
        let s = s . insertion
        let i = next
      endif
    else
      let s = s . char
    endif
    let i = i + 1
  endwhile
  return s
endfunction

" }}}2

function! s:wrap(sobj, inner, type, special, indent)
  let left = a:sobj['left']
  let right = a:sobj['right']
  let spaces = repeat(' ', a:sobj['nspaces'])

  if a:type =~ "^\<C-V>"
    let width = matchstr(a:type, '\d\+$')
    let lines = split(a:inner, "\n")
    for i in range(len(lines))
      let diff = width - len(lines[i])
      if diff > 0
        let lines[i] .= repeat(' ', diff)
      endif
      let lines[i] = left . spaces . lines[i] . spaces . right
    endfor
    return join(lines, "\n")
  endif

  let left  = substitute(left, '\n\ze\_S', '\n' . a:indent, 'g')
  let right = substitute(right,'\n\ze\_S', '\n' . a:indent, 'g')

  let left  = substitute(left, '^\n\s\+', '\n', '')
  let right = substitute(right,'^\n\s\+', '\n', '')

  if !a:special
    let left  = left . spaces
    let right = spaces . right
  else
    "
    " Fixup the beginning of the 'left'
    "
    if left =~ '^\n'
      if a:type ==# 'V'
        " In line-wise mode, ignore the newline at the beginning of 'left'
        " because it already exists.  This also means "don't indent 'left'."
        let left = strpart(left, 1)
      endif
    else
      if a:type ==# 'V'
        " In line-wise mode, indent 'left' at the same level of first line.
        let left = a:indent . left
      endif
    endif

    "
    " Fixup the end of the 'left'
    "
    if left !~ '\n$'
      " In special mode, a newlines is required left the content.
      let left = substitute(left,' \+$','','') . "\n"
    endif
    if a:type ==# 'v'
      " In char-wise mode, indent the content.
      let left = left . a:indent
    endif

    "
    " Fixup the beginning of the 'right'
    "
    if right =~ '^\n'
      if a:type ==# 'V'
        " In line-wise mode, ignore the newline at the beginning of the
        " 'right'.  because it already exists.  This also means "don't indent
        " 'right'."
        let right = strpart(right, 1)
      endif
    else
      " In special mode, indent the 'right' at the same level of first line.
      let right  = a:indent . substitute(right ,'^ \+','','')
      if a:type ==# 'v'
        " In char-wise mode, a newline is required right the content.
        let right = "\n" . right
      endif
    endif

    "
    " Fixup the end of the 'right'
    "
    if right !~ '\n$'
      if a:type ==# 'V'
        " In line-wise mode, add the newline at the end of 'right'.
        let right = right . "\n"
      endif
    endif
  endif

  return left . a:inner . right
endfunction

function! s:wrapreg(sobj, reg, ...)
  let orig = getreg(a:reg)
  "let type = substitute(getregtype(a:reg), '\d\+$', '', '')
  let type = getregtype(a:reg)
  let special = a:0 > 0 ? a:1 : 0
  let indent = a:0 > 1 ? a:2 : ''
  let new = s:wrap(a:sobj, orig, type, special, indent)
  call setreg(a:reg, new, type)
endfunction

" }}}1

" Insert mode functions {{{1

function! s:insert(...)
  " Optional argument causes the result to appear on 3 lines, not 1
  "call inputsave()
  let linemode = a:0 ? a:1 : 0
  let char = s:inputreplacement()
  while char == "\<CR>" || char == "\<C-S>"
    " TODO: use total count for additional blank lines
    let linemode = linemode + 1
    let char = s:inputreplacement()
  endwhile
  "call inputrestore()
  if char == ""
    return ""
  endif
  "call inputsave()
  let cb_save = &clipboard
  set clipboard-=unnamed
  let reg_save = @@
  call setreg('"',"\r",'v')
  let sobj = s:get_surround_object(char)
  call s:wrapreg(sobj,'"',linemode)
  " If line mode is used and the surrounding consists solely of a suffix,
  " remove the initial newline.  This fits a use case of mine but is a
  " little inconsistent.  Is there anyone that would prefer the simpler
  " behavior of just inserting the newline?
  if linemode && match(getreg('"'),'^\n\s*\zs.*') == 0
    call setreg('"',matchstr(getreg('"'),'^\n\s*\zs.*'),getregtype('"'))
  endif
  " This can be used to append a placeholder to the end
  if exists("g:surround_insert_tail")
    call setreg('"',g:surround_insert_tail,"a".getregtype('"'))
  endif
  "if linemode
  "call setreg('"',substitute(getreg('"'),'^\s\+','',''),'c')
  "endif
  if col('.') >= col('$')
    norm! ""p
  else
    norm! ""P
  endif
  if linemode
    call s:reindent(sobj)
  endif
  norm! `]
  call search('\r','bW')
  let @@ = reg_save
  let &clipboard = cb_save
  return "\<Del>"
endfunction

" }}}1

" Normal and Visual mode functions {{{1

function! s:dosurround(...)
  let tseq = a:0 > 0 ? a:1 : s:inputtarget()
  let rseq = a:0 > 1 ? a:2 : ''

  let mlist = matchlist(tseq, '\v^(\d*)( ?)(.*)$')
  let tcount = v:count1 * (mlist[1] == "" ? 1 : mlist[1])
  let tspace = mlist[2] == ' '
  let tseq = mlist[3]

  if tseq == ''
    return
  endif

  let sel_save = &selection
  let &selection = "inclusive"

  let pos_save = getpos(".")
  let reg_save = [getreg('a'), getregtype('a')]
  call setreg('a', '')

  " Don't use normal! for user-defined objects.
  execute 'silent normal "ay' . tcount . 'a' . tseq
  if getreg('a') == ''
    " No text-object found.
    call setreg('a', reg_save[0], reg_save[1])
    return
  endif
  let outer_pos = [getpos("'["), getpos("']")]

  call setpos(".", pos_save)

  " Don't use normal! for user-defined objects.
  execute 'silent normal "ay' . tcount . 'i' . tseq
  let inner = getreg('a')
  let innertype = getregtype('a')

  call setpos(".", pos_save)

  let before = ''
  let after = ''

  if tseq =~# '^[Wpsw]$'
    " Adjust non-block objects.
    " thi*s is example  ; * is cursor, here
    "   2daw -> example (deleted "this is ")
    "   2diw -> is example (deleted "this ")
    " We prefer to wrap 2 words with 2csw command.
    execute 'silent normal "ad' . tcount . 'a' . tseq

    let outer = getreg('a')
    let outertype = getregtype('a')

    let mlist = matchlist(outer, '^\v(\_s*)(.{-})(\_s*)$')
    let before = mlist[1]
    let inner = mlist[2]
    let after = mlist[3]
    let surrounds = ['', '']
  else
    " For block objects, we assume that difference in 'inner' and 'outer'
    " objects is surrounding object.
    if tspace && innertype ==# 'V'
      call setpos('.', outer_pos[0])
      call search('\%(^\|\S\)\zs\s*\%#')
      normal! m[
      call setpos('.', outer_pos[1])
      call search('\%#.\s*\ze\%($\|\S\)', 'e')
      let delete_space = (col('.') + 1 == col('$'))
      normal! v`["ad
    else
      " Don't use normal! for user-defined objects
      execute 'silent normal "ad' . tcount . 'a' . tseq
    endif

    let outer = getreg('a')
    let outertype = getregtype('a')

    let idx = (inner == '') ? 1 : stridx(outer, inner)
    let len = strlen(inner)
    let surrounds = [strpart(outer, 0, idx), strpart(outer, idx + len)]

    if tspace
      if innertype ==# 'V'
        let mlist = matchlist(surrounds[0], '^\v(.{-})(\_s*)$')
        let surrounds[0] = mlist[1]
        let inner = mlist[2] . inner

        let mlist = matchlist(surrounds[1], '^\v(\_s*)(.{-})$')
        if delete_space
          let surrounds[1] = mlist[1] . mlist[2]
        else
          let inner = inner . mlist[1]
          let surrounds[1] = mlist[2]
        endif
      else " innertype ==# 'v'
        let mlist = matchlist(inner, '^\v(\s*)(.{-})(\s*)$')
        let inner = mlist[2]
        let surrounds[0] = surrounds[0] . mlist[1]
        let surrounds[1] = mlist[3] . surrounds[1]
      endif
      if surrounds[1][0] == "\n"
        let surrounds[1] = surrounds[1][1:]
        let after = "\n"
      endif
      if surrounds[1][-1] == "\n"
        let surrounds[1] = surrounds[1][:-2]
        let after = "\n"
      endif
    else
      let mlist = matchlist(surrounds[0], '^\v(\_s*)(.{-})(\_s*)$')
      let before = mlist[1]
      let surrounds[0] = mlist[2]
      let inner = mlist[3] . inner

      let mlist = matchlist(surrounds[1], '^\v(\_s*)(.{-})(\_s*)$')
      let inner = inner . mlist[1]
      let surrounds[1] = mlist[2]
      let after = mlist[3]
    endif
  endif

  let pcmd = s:getpcmd(outertype)

  call setreg('a', inner, 'v')
  if rseq != ""
    let sobj = s:get_surround_object(rseq)
    call s:wrapreg(sobj, 'a')
  endif
  call setreg('a', before . getreg('a') . after, outertype)

  undojoin
  execute 'silent normal! "a' . pcmd

  if before != ''
    normal! `[
    execute 'normal! ' . strlen(before) . 'l'
    normal! m[
  endif

  if after != ''
    normal! `]
    execute 'normal! ' . strlen(after) . 'h'
    normal! m]
  endif

  normal! `[

  let s:lastdel = join(surrounds, '')
  call setreg('"', s:lastdel)

  call setreg('a', reg_save[0], reg_save[1])
  let &selection = sel_save

  if rseq == ""
    silent! call repeat#set("\<Plug>Dsurround" . tseq, tcount)
  else
    silent! call repeat#set("\<Plug>Csurround" . tseq . rseq, tcount)
  endif
endfunction

function! s:changesurround()
  let a = s:inputtarget()
  if a == ""
    return ""
  endif
  let b = s:inputreplacement()
  if b == ""
    return ""
  endif
  call s:dosurround(a,b)
endfunction

function! s:opfunc(type, ...)
  let blockmode = a:0 > 0 ? a:1 : 0

  let rseq = s:inputreplacement()
  if rseq == ""
    return ""
  endif

  let reg_save = [getreg('a'), getregtype('a')]
  let sel_save = &selection
  let cb_save  = &clipboard

  let reg = 'a'
  let &selection = "inclusive"
  set clipboard-=unnamed clipboard-=unnamedplus

  if a:type == "char"
    let indent = matchstr(getline("'["), '^\s*')
    silent normal! `[v`]"ad
  elseif a:type == "line"
    let indent = matchstr(getline("'["), '^\s*')
    silent normal! `[V`]"ad
  elseif a:type =~ '^\d\+$'
    let indent = matchstr(getline("."), '^\s*')
    execute 'silent normal! "a' . a:type . 'dd'
  elseif a:type ==# "v" || a:type ==# "V" || a:type ==# "\<C-V>"
    let indent = matchstr(getline("'<"), '^\s*')
    execute 'silent normal! `<' . a:type . '`>"ad'
  else
    let &selection = sel_save
    let &clipboard = cb_save
    return ""
  endif

  let type = getregtype('a')
  let otype = type
  let inner = getreg('a')
  let before = ''
  let after = ''

  if type ==# 'v' || (type ==# 'V' && !blockmode)
    let type = 'v'
    let mlist = matchlist(inner, '^\(\s*\)\(.\{-\}\)\(\n\?\s*\)$')
    let inner = mlist[2]
    let before = mlist[1]
    let after = mlist[3]
  endif

  call setreg('a', inner, type)
  let sobj = s:get_surround_object(rseq)
  call s:wrapreg(sobj, 'a', blockmode, indent)

  call setreg('a', before . getreg('a') . after, otype)
  let pcmd = s:getpcmd(otype)
  silent execute 'normal! "a' . pcmd . '`['

  if type ==# 'V' && blockmode && sobj['nspaces'] > 0
    let [spos, epos] = [getpos("'["), getpos("']")]

    if sobj['nspaces'] > 1
      call setpos('.', epos)
      let trim = getline(line('.') + 1) =~# '^\s*$'
      normal! J
      if trim && getline('.')[col('.')-1] =~ '\s'
        normal! x
      endif
      let epos = getpos(".")
    endif

    call setpos('.', spos)
    normal! kJm[

    let epos[1] -= 1
    call setpos("']", epos)
  endif

  if blockmode
    call s:reindent(sobj)
  endif

  let &clipboard = cb_save
  let &selection = sel_save
  call setreg('a', reg_save[0], reg_save[1])

  if a:type =~ '^\d\+$'
    silent! call repeat#set(
    \ "\<Plug>Y" . (blockmode ? "gs" : "s") . "surround" . rseq, a:type)
  else
    silent! call repeat#set("\<Plug>SurroundRepeat" . rseq)
  endif
endfunction

function! s:opfunc2(arg)
  call s:opfunc(a:arg,1)
endfunction

function! s:getpcmd(type)
  if a:type ==# 'V'
    if line("']") == line("$") + 1 && line(".") == line("$")
      return 'p'
    endif
  else
    if col("']") == col("$")
      return 'p'
    endif
  endif
  return 'P'
endfunction

function! s:reindent(sobj)
  if has_key(a:sobj, 'reindent') ? a:sobj['reindent'] :
  \  (exists("b:surround_indent") ? b:surround_indent : g:surround_indent)
    silent normal! '[=']
  endif
endfunction

" }}}1

let s:surround_default_objects = {
\  'b': {'left': '(', 'right': ')', 'nspaces': 0},
\  '(': {'left': '(', 'right': ')', 'nspaces': 0},
\  ')': {'left': '(', 'right': ')', 'nspaces': 1},
\  'B': {'left': '{', 'right': '}', 'nspaces': 0},
\  '{': {'left': '{', 'right': '}', 'nspaces': 0},
\  '}': {'left': '{', 'right': '}', 'nspaces': 1},
\  'r': {'left': '[', 'right': ']', 'nspaces': 0},
\  '[': {'left': '[', 'right': ']', 'nspaces': 0},
\  ']': {'left': '[', 'right': ']', 'nspaces': 1},
\  'a': {'left': '<', 'right': '>', 'nspaces': 0},
\  '<': {'left': '<', 'right': '>', 'nspaces': 0},
\  '>': {'left': '<', 'right': '>', 'nspaces': 1},
\  'p': {'left': "\n\n", 'right': "\n\n", 'nspaces': 0},
\  's': {'left': ' ', 'right': ' ', 'nspaces': 0},
\  't': {'left': "<\1>", 'right': "</\2>", 'nspaces': 0,
\        'inputfunc': function('s:tag_input')},
\  'T': {'left': "<\1>", 'right': "</\2>", 'nspaces': 0,
\        'inputfunc': function('s:tag_input_lastdel')},
\  ',': {'left': "<\1>\n\t", 'right': "\n</\2>", 'nspaces': 0,
\        'inputfunc': function('s:tag_input')},
\  'l': {'left': "\\begin\1", 'right': "\\end\2", 'nspaces': 0,
\        'inputfunc': function('s:latex_input')},
\  '\': {'left': "\\begin\1", 'right': "\\end\2", 'nspaces': 0,
\        'inputfunc': function('s:latex_input')},
\  'f': {'left': "\1(", 'right': ')', 'nspaces': 0,
\        'inputfunc': function('s:simple_input'),
\        'inputfuncargs': ['function: ']},
\  'F': {'left': "\1(", 'right': ')', 'nspaces': 1,
\        'inputfunc': function('s:simple_input'),
\        'inputfuncargs': ['function: ']},
\}

if !exists("g:surround_objects")
  let g:surround_objects = {}
endif
if !exists("g:surround_no_default_objects") || !g:surround_no_default_objects
  call extend(g:surround_objects, s:surround_default_objects, "keep")
endif
if !exists("g:surround_indent")
  let g:surround_indent = 1
endif

nnoremap <silent> <Plug>SurroundRepeat .
nnoremap <silent> <Plug>Dsurround   :<C-u>call <SID>dosurround()<CR>
nnoremap <silent> <Plug>Csurround   :<C-u>call <SID>changesurround()<CR>
nnoremap <silent> <Plug>Yssurround  :<C-u>call <SID>opfunc(v:count1)<CR>
nnoremap <silent> <Plug>Ygssurround :<C-u>call <SID>opfunc2(v:count1)<CR>
nnoremap <silent> <Plug>Ysurround   :<C-u>set opfunc=<SID>opfunc<CR>g@
nnoremap <silent> <Plug>Ygsurround  :<C-u>set opfunc=<SID>opfunc2<CR>g@
vnoremap <silent> <Plug>Vsurround   :<C-u>call <SID>opfunc(visualmode())<CR>
vnoremap <silent> <Plug>VSurround   :<C-u>call <SID>opfunc('V')<CR>
vnoremap <silent> <Plug>Vgsurround  :<C-u>call <SID>opfunc2(visualmode())<CR>
vnoremap <silent> <Plug>VgSurround  :<C-u>call <SID>opfunc2('V')<CR>
vnoremap <silent> <Plug>oVSurround  :<C-u>call <SID>opfunc(visualmode(), visualmode() ==# 'V' ? 1 : 0)<CR>
vnoremap <silent> <Plug>oVgSurround :<C-u>call <SID>opfunc(visualmode(), visualmode() ==# 'V' ? 0 : 1)<CR>
inoremap <silent> <Plug>Isurround   <C-R>=<SID>insert()<CR>
inoremap <silent> <Plug>ISurround   <C-R>=<SID>insert(1)<CR>

if !exists("g:surround_no_mappings") || ! g:surround_no_mappings
  if exists("g:surround_old_mappings") && g:surround_old_mappings
    silent! nmap <unique> ds     <Plug>Dsurround
    silent! nmap <unique> cs     <Plug>Csurround
    silent! nmap <unique> ys     <Plug>Ysurround
    silent! nmap <unique> yS     <Plug>Ygsurround
    silent! nmap <unique> yss    <Plug>Yssurround
    silent! nmap <unique> ySs    <Plug>Ygssurround
    silent! nmap <unique> ySS    <Plug>Ygssurround
    silent! xmap <unique> s      <Plug>Vsurround
    silent! xmap <unique> S      <Plug>oVSurround
    silent! xmap <unique> gS     <Plug>oVgSurround
    silent! imap <unique> <C-S>  <Plug>Isurround
    silent! imap <unique> <C-G>s <Plug>Isurround
    silent! imap <unique> <C-G>S <Plug>ISurround
  else
    silent! nmap <unique> ds     <Plug>Dsurround
    silent! nmap <unique> cs     <Plug>Csurround
    silent! nmap <unique> ys     <Plug>Ysurround
    silent! nmap <unique> yS     <Plug>Ysurround$
    silent! nmap <unique> yss    <Plug>Yssurround
    silent! nmap <unique> ygs    <Plug>Ygsurround
    silent! nmap <unique> ygS    <Plug>Ygsurround$
    silent! nmap <unique> ygss   <Plug>Ygssurround
    silent! nmap <unique> ygsgs  <Plug>Ygssurround
    silent! xmap <unique> s      <Plug>Vsurround
    silent! xmap <unique> S      <Plug>VSurround
    silent! xmap <unique> gs     <Plug>Vgsurround
    silent! xmap <unique> gS     <Plug>VgSurround
    silent! imap <unique> <C-S>  <Plug>Isurround
    silent! imap <unique> <C-G>s <Plug>Isurround
    silent! imap <unique> <C-G>S <Plug>ISurround
  endif
endif

let &cpo = s:cpo_save

" vim:set ft=vim sw=2 sts=2 et:
