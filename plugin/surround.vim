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


" Insert mode functions {{{1

function! s:insert(...)
  " Optional argument causes the result to appear on 3 lines, not 1
  "call inputsave()
  let linemode = a:0 ? a:1 : 0
  let rseq = surround#inputreplacement()
  while rseq[2] == "\<CR>" || rseq[2] == "\<C-S>"
    " TODO: use total count for additional blank lines
    let linemode = linemode + 1
    let rseq = surround#inputreplacement()
  endwhile
  "call inputrestore()
  if rseq == []
    return ""
  endif
  "call inputsave()
  let cb_save = &clipboard
  set clipboard-=unnamed
  let reg_save = @@
  let sobj = surround#resolve(surround#get(rseq))
  if surround#is_null(sobj)
    return ""
  endif
  call setreg('"',"\r",'v')
  call surround#wrapreg(sobj,'"',linemode)
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
  let target = a:0 > 0 ? a:1 : surround#inputtarget()
  let rseq = a:0 > 1 ? a:2 : []

  if target == []
    return
  endif

  let tcount = v:count1 * (target[0] == "" ? 1 : target[0])
  let tspace = len(target[1])
  let tseq = target[2]

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
    call setpos(".", pos_save)
    return
  endif
  let outer_pos = [getpos("'["), getpos("']")]

  if rseq != []
    let sobj = surround#resolve(surround#get(rseq))
    if surround#is_null(sobj)
      call setreg('a', reg_save[0], reg_save[1])
      call setpos(".", pos_save)
      return
    endif
  endif

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

  if rseq != []
    let new = surround#wrap(sobj, inner, 'v', 0, '')
  else
    let new = inner
  endif
  call setreg('a', before . new . after, outertype)

  undojoin
  execute 'silent normal! "a' . pcmd

  let ww_save = &whichwrap
  let &whichwrap = "b,s"

  if before != ''
    normal! `[
    execute 'normal! ' . strlen(before) . "\<Space>"
    normal! m[
  endif

  if after != ''
    normal! `]
    execute 'normal! ' . strlen(after) . "\<BS>"
    normal! m]
  endif

  let &whichwrap = ww_save

  normal! `[

  let b:surround_lastdel = join(surrounds, '')
  call setreg('"', b:surround_lastdel)

  call setreg('a', reg_save[0], reg_save[1])
  let &selection = sel_save

  if rseq == []
    silent! call repeat#set("\<Plug>Dsurround" . join(target, ''), tcount)
  else
    silent! call repeat#set("\<Plug>Csurround" . join(target, '') .
    \                       join(rseq, ''), tcount)
  endif
endfunction

function! s:changesurround()
  let a = surround#inputtarget()
  if a == []
    return ""
  endif
  let b = surround#inputreplacement()
  if b == []
    return ""
  endif
  call s:dosurround(a,b)
endfunction

function! s:opfunc(type, ...)
  let blockmode = a:0 > 0 ? a:1 : 0

  let rseq = surround#inputreplacement()
  if rseq == []
    return ""
  endif

  let sobj = surround#resolve(surround#get(rseq))
  if surround#is_null(sobj)
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
    let ve_save = &virtualedit
    if !blockmode
      let &virtualedit = ''
    endif
    execute 'silent normal! `<' . a:type . '`>"ad'
    let &virtualedit = ve_save
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

  let new = surround#wrap(sobj, inner, type, blockmode, indent)

  call setreg('a', before . new . after, otype)
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
    \ "\<Plug>Y" . (blockmode ? "gs" : "s") . "surround" . join(rseq, ''),
    \ a:type)
  else
    silent! call repeat#set("\<Plug>SurroundRepeat" . join(rseq, ''))
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
\        'inputfunc': 'surround#tag_input'},
\  'T': {'left': "<\1>", 'right': "</\2>", 'nspaces': 0,
\        'inputfunc': 'surround#tag_input_lastdel'},
\  ',': {'left': "<\1>\n\t", 'right': "\n</\2>", 'nspaces': 0,
\        'inputfunc': 'surround#tag_input'},
\  'l': {'left': "\\begin\1", 'right': "\\end\2", 'nspaces': 0,
\        'inputfunc': 'surround#latex_input'},
\  '\': {'left': "\\begin\1", 'right': "\\end\2", 'nspaces': 0,
\        'inputfunc': 'surround#latex_input'},
\  'f': {'left': "\1(", 'right': ')', 'nspaces': 0,
\        'inputfunc': 'surround#simple_input',
\        'inputfuncargs': ['function: ']},
\  'F': {'left': "\1(", 'right': ')', 'nspaces': 1,
\        'inputfunc': 'surround#simple_input',
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
if !exists("g:surround_ignore_target_count")
  let g:surround_ignore_target_count = 0
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
