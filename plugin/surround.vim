" surround.vim - Surroundings
" Author:       Tim Pope <vimNOSPAM@tpope.org>
" Version:      1.90
" GetLatestVimScripts: 1697 1 :AutoInstall: surround.vim
"
" See surround.txt for help.  This can be accessed by doing
"
" :helptags ~/.vim/doc
" :help surround
"
" Licensed under the same terms as Vim itself.

" ============================================================================

" Exit quickly when:
" - this plugin was already loaded or disabled
" - when 'compatible' is set
if (exists("g:loaded_surround") && g:loaded_surround) || &cp
  finish
endif
let g:loaded_surround = 1

let s:cpo_save = &cpo
set cpo&vim

" Input functions {{{1

function! s:getchar()
  let c = getchar()
  if c =~ '^\d\+$'
    let c = nr2char(c)
  endif
  return c
endfunction

function! s:inputtarget()
  let c = s:getchar()
  while c =~ '^\d\+$'
    let c = c . s:getchar()
  endwhile
  if c == " "
    let c = c . s:getchar()
  endif
  if c =~ "\<Esc>\|\<C-C>\|\0"
    return ""
  else
    return c
  endif
endfunction

function! s:inputreplacement()
  "echo '-- SURROUND --'
  let c = s:getchar()
  if c == " "
    let c = c . s:getchar()
  endif
  if c =~ "\<Esc>" || c =~ "\<C-C>"
    return ""
  else
    return c
  endif
endfunction

function! s:beep()
  exe "norm! \<Esc>"
  return ""
endfunction

function! s:redraw()
  redraw
  return ""
endfunction

" }}}1

" Wrapping functions {{{1

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

function! s:repeat(str,count)
  let cnt = a:count
  let str = ""
  while cnt > 0
    let str = str . a:str
    let cnt = cnt - 1
  endwhile
  return str
endfunction

function! s:fixindent(str,spc)
  let str = substitute(a:str,'\t',s:repeat(' ',&sw),'g')
  let spc = substitute(a:spc,'\t',s:repeat(' ',&sw),'g')
  let str = substitute(str,'\(\n\|\%^\).\@=','\1'.spc,'g')
  if ! &et
    let str = substitute(str,'\s\{'.&ts.'\}',"\t",'g')
  endif
  return str
endfunction

function! s:process(string)
  let i = 0
  while i < 7
    let i = i + 1
    let repl_{i} = ''
    let m = matchstr(a:string,nr2char(i).'.\{-\}\ze'.nr2char(i))
    if m != ''
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

function! s:wrap(string,char,type,...)
  let keeper = a:string
  let newchar = a:char
  let type = a:type
  let linemode = type ==# 'V' ? 1 : 0
  let special = a:0 ? a:1 : 0
  let before = ""
  let after  = ""
  if type ==# "V"
    let initspaces = matchstr(keeper,'\%^\s*')
  else
    let initspaces = matchstr(getline('.'),'\%^\s*')
  endif
  " Duplicate b's are just placeholders (removed)
  let pairs = "b()B{}r[]a<>"
  let extraspace = ""
  if newchar =~ '^ '
    let newchar = strpart(newchar,1)
    let extraspace = ' '
  endif
  let idx = stridx(pairs,newchar)
  if newchar == ' '
    let before = ''
    let after  = ''
  elseif exists("b:surround_".char2nr(newchar))
    let all    = s:process(b:surround_{char2nr(newchar)})
    let before = s:extractbefore(all)
    let after  =  s:extractafter(all)
  elseif exists("g:surround_".char2nr(newchar))
    let all    = s:process(g:surround_{char2nr(newchar)})
    let before = s:extractbefore(all)
    let after  =  s:extractafter(all)
  elseif newchar ==# "p"
    let before = "\n"
    let after  = "\n\n"
  elseif newchar =~# "[tT\<C-T><,]"
    let dounmapp = 0
    let dounmapb = 0
    if !maparg(">","c")
      let dounmapb= 1
      " Hide from AsNeeded
      exe "cn"."oremap > <CR>"
    endif
    let default = ""
    if newchar ==# "T"
      if !exists("s:lastdel")
        let s:lastdel = ""
      endif
      let default = matchstr(s:lastdel,'<\zs.\{-\}\ze>')
    endif
    let tag = input("<",default)
    echo "<".substitute(tag,'>*$','>','')
    if dounmapb
      silent! cunmap >
    endif
    if tag != ""
      let tag = substitute(tag,'>*$','','')
      let before = '<'.tag.'>'
      if tag =~ '/$'
        let after = ''
      else
        let after  = '</'.substitute(tag,' .*','','').'>'
      endif
      if newchar == "\<C-T>" || newchar == ","
        if type ==# "v" || type ==# "V"
          let before = before . "\n\t"
        endif
        if type ==# "v"
          let after  = "\n". after
        endif
      endif
    endif
  elseif newchar ==# 'l' || newchar == '\'
    " LaTeX
    let env = input('\begin{')
    let env = '{' . env
    let env = env . s:closematch(env)
    echo '\begin'.env
    if env != ""
      let before = '\begin'.env
      let after  = '\end'.matchstr(env,'[^}]*').'}'
    endif
    "if type ==# 'v' || type ==# 'V'
    "let before = before ."\n\t"
    "endif
    "if type ==# 'v'
    "let after  = "\n".initspaces.after
    "endif
  elseif newchar ==# 'f' || newchar ==# 'F'
    let fnc = input('function: ')
    if fnc != ""
      let before = substitute(fnc,'($','','').'('
      let after  = ')'
      if newchar ==# 'F'
        let before = before . ' '
        let after  = ' ' . after
      endif
    endif
  elseif idx >= 0
    let spc = (idx % 3) == 1 ? " " : ""
    let idx = idx / 3 * 3
    let before = strpart(pairs,idx+1,1) . spc
    let after  = spc . strpart(pairs,idx+2,1)
  elseif newchar == "\<C-[>" || newchar == "\<C-]>"
    let before = "{\n\t"
    let after  = "\n}"
  elseif newchar !~ '\a'
    let before = newchar
    let after  = newchar
  else
    let before = ''
    let after  = ''
  endif
  "let before = substitute(before,'\n','\n'.initspaces,'g')
  let after  = substitute(after ,'\n','\n'.initspaces,'g')
  "let after  = substitute(after,"\n\\s*\<C-U>\\s*",'\n','g')
  if type ==# 'V' || (special && type ==# "v")
    let before = substitute(before,' \+$','','')
    let after  = substitute(after ,'^ \+','','')
    if after !~ '^\n'
      let after  = initspaces.after
    endif
    if keeper !~ '\n$' && after !~ '^\n'
      let keeper = keeper . "\n"
    elseif keeper =~ '\n$' && after =~ '^\n'
      let after = strpart(after,1)
    endif
    if before !~ '\n\s*$'
      let before = before . "\n" . initspaces
    endif
  endif
  if type ==# 'V'
    let before = initspaces.before
  endif
  if before =~ '\n\s*\%$'
    if type ==# 'v'
      let keeper = initspaces.keeper
    endif
    let padding = matchstr(before,'\n\zs\s\+\%$')
    let before  = substitute(before,'\n\s\+\%$','\n','')
    let keeper = s:fixindent(keeper,padding)
  endif
  if type ==# 'V'
    let keeper = before.keeper.after
  elseif type =~ "^\<C-V>"
    " Really we should be iterating over the buffer
    let repl = substitute(before,'[\\~]','\\&','g').'\1'.substitute(after,'[\\~]','\\&','g')
    let repl = substitute(repl,'\n',' ','g')
    let keeper = substitute(keeper."\n",'\(.\{-\}\)\(\n\)',repl.'\n','g')
    let keeper = substitute(keeper,'\n\%$','','')
  else
    let keeper = before.extraspace.keeper.extraspace.after
  endif
  return keeper
endfunction

function! s:wrapreg(reg,char,...)
  let orig = getreg(a:reg)
  let type = substitute(getregtype(a:reg),'\d\+$','','')
  let special = a:0 ? a:1 : 0
  let new = s:wrap(orig,a:char,type,special)
  call setreg(a:reg,new,type)
endfunction
" }}}1

function! s:insert(...) " {{{1
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
  call s:wrapreg('"',char,linemode)
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
    call s:reindent()
  endif
  norm! `]
  call search('\r','bW')
  let @@ = reg_save
  let &clipboard = cb_save
  return "\<Del>"
endfunction " }}}1

function! s:reindent() " {{{1
  if exists("b:surround_indent") ? b:surround_indent : (exists("g:surround_indent") && g:surround_indent)
    silent norm! '[=']
  endif
endfunction " }}}1

function! s:dosurround(...) " {{{1
  let scount = v:count1
  let char = (a:0 ? a:1 : s:inputtarget())
  let spc = ""
  if char =~ '^\d\+'
    let scount = scount * matchstr(char,'^\d\+')
    let char = substitute(char,'^\d\+','','')
  endif
  let strcount = (scount == 1 ? "" : scount)
  if char =~ '^ '
    let char = strpart(char,1)
    let spc = 1
  endif
  let newchar = ""
  if a:0 > 1
    let newchar = a:2
    if newchar == "\<Esc>" || newchar == "\<C-C>" || newchar == ""
      return s:beep()
    endif
  endif

  let cb_save = &clipboard
  set clipboard-=unnamed
  let original = getreg('"')
  let otype = getregtype('"')
  call setreg('"',"")

  let pos = getpos('.')
  " Don't use normal! for user-defined objects
  exe 'norm y'.strcount.'i'.char
  call setpos('.', pos)
  let inner = getreg('"')
  if inner == ""
    call setreg('"',original,otype)
    let &clipboard = cb_save
    return ""
  endif

  let white_after = ''
  let white_before = ''

  if char =~# '[Wpsw]'
    " Non-block objects have strange behavior for counting.
    " thi*s is example  ; * is cursor, here
    "   2daw -> example (deleted "this is ")
    "   2diw -> is example (deleted "this ")
    " We prefer to wrap 2 words with 2csw command.
    exe 'norm! '.strcount.'da'.char
    let mlist = matchlist(getreg('"'), '^\(\s*\)\(.\{-\}\)\(\n*\s*\)$')
    let keeper = mlist[2]
    let white_before = mlist[1]
    let white_after = mlist[3]
    let ma = ''
    let mb = ''
  else
    " For block objects, we assume that difference in 'inner' and 'outer'
    " objects is surrounding symbol.
    if spc
      " Don't use normal! for user-defined objects
      exe "norm ".strcount."ya".char
      norm! `[
      call search('\S', 'b', line('.'))
      if col('.') != col("'[")
        exe "norm! lm["
      endif
      norm! `]
      call search('\S', '', line('.'))
      if col('.') != col("']")
        exe "norm! hm]"
      endif
      exe "norm! `[v`]d"
    else
      " Don't use normal! for user-defined objects
      exe "norm ".strcount."da".char
    endif
    let keeper = getreg('"')
    let ma = keeper[:(stridx(keeper, inner) - 1)]
    let mb = keeper[(strlen(ma) + strlen(inner)):]
    let ma = substitute(ma, '^\v\_s*(.{-})\_s*$', '\1', '')
    let mb = substitute(mb, '^\v\_s*(.{-})\_s*$', '\1', '')
  endif

  let mlist = matchlist(keeper,
        \'^\v(\s*)(\V'.ma.'\v)(\s*)(.{-})(\s*)(\V'.mb.'\v)(\n?)(\s*)$')
  if spc
    let white_after = mlist[7] . white_after
    let keeper = mlist[4]
    call setreg('"', mlist[1].mlist[2].mlist[3].mlist[5].mlist[6].mlist[8])
  else
    let white_before = white_before . mlist[1]
    let white_after = mlist[7] . mlist[8] . white_after
    let keeper = mlist[3] . mlist[4] . mlist[5]
    call setreg('"', mlist[2] . mlist[6])
  end

  let removed = getreg('"')
  let regtype = getregtype('"')
  if col("']") == col("$") && col('.') + 1 == col('$')
    let pcmd = "p"
  else
    let pcmd = "P"
  endif

  call setreg('"',keeper,regtype)
  if newchar != ""
    call s:wrapreg('"',newchar)
  endif
  call setreg('"', white_before . getreg('"') . white_after, regtype)

  silent exe 'norm! '.pcmd.'`['
  if white_before != ''
    let scount = strlen(white_before)
    let strcount = scount == 1 ? '' : scount
    silent exe 'norm! '.strcount.'l'
  endif
  if removed =~ '\n' || getreg('"') =~ '\n'
    call s:reindent()
  endif

  call setreg('"',removed,regtype)
  let s:lastdel = removed
  let &clipboard = cb_save

  if newchar == ""
    silent! call repeat#set("\<Plug>Dsurround".char,scount)
  else
    silent! call repeat#set("\<Plug>Csurround".char.newchar,scount)
  endif
endfunction " }}}1

function! s:changesurround() " {{{1
  let a = s:inputtarget()
  if a == ""
    return s:beep()
  endif
  let b = s:inputreplacement()
  if b == ""
    return s:beep()
  endif
  call s:dosurround(a,b)
endfunction " }}}1

function! s:opfunc(type,...) " {{{1
  let char = s:inputreplacement()
  if char == ""
    return s:beep()
  endif
  let reg = 'a'
  let sel_save = &selection
  let &selection = "inclusive"
  let cb_save  = &clipboard
  set clipboard-=unnamed
  let reg_save = getreg(reg)
  let reg_type = getregtype(reg)
  let blockmode = a:0 && a:1
  if a:type == "char"
    silent exe 'norm! `[v`]"'.reg.'d'
  elseif a:type == "line"
    silent exe 'norm! `[V`]"'.reg.'d'
  elseif a:type =~ '^\d\+$'
    silent exe 'norm! "'.reg.a:type.'dd'
  elseif a:type ==# "v" || a:type ==# "V" || a:type ==# "\<C-V>"
    let ve = &virtualedit
    if !(a:0 && a:1)
      set virtualedit=
    endif
    silent exe 'norm! `<'.a:type.'`>"'.reg.'d'
    let &virtualedit = ve
  else
    let &selection = sel_save
    let &clipboard = cb_save
    return s:beep()
  endif
  let type = getregtype(reg)
  let otype = type
  let keeper = getreg(reg)
  let before = ''
  let after = ''
  if type ==# 'v' || (type ==# 'V' && !blockmode)
    let type = 'v'
    let mlist = matchlist(keeper, '^\(\s*\)\(.\{-\}\)\(\n\?\s*\)$')
    let keeper = mlist[2]
    let before = mlist[1]
    let after = mlist[3]
  endif
  call setreg(reg,keeper,type)
  call s:wrapreg(reg,char,blockmode)
  call setreg(reg,before.getreg(reg).after,otype)
  let pcmd = (col("']") == col("$") && col('.') + 1 == col('$')) ? 'p' : 'P'
  exe 'norm! "'.reg.pcmd.'`['
  if type ==# 'V' || (getreg(reg) =~ '\n' && type ==# 'v')
    call s:reindent()
  endif
  if a:0 && a:1 && char =~ '^ '
    let spos = getpos("'[")
    let epos = getpos("']")

    call setpos('.', epos)
    let trim = (match(getline(line('.') + 1), '^\s*$') != -1)
    norm! J
    if trim && getline('.')[col('.')-1] =~ '\s'
      norm! x
    endif
    let epos = getpos(".")

    call setpos('.', spos)
    norm! kJm[

    let epos[1] -= 1
    call setpos("']", epos)
  endif
  call setreg(reg,reg_save,reg_type)
  let &selection = sel_save
  let &clipboard = cb_save
  if a:type =~ '^\d\+$'
    silent! call repeat#set("\<Plug>Y".(a:0 && a:1 ? "gs" : "s")."surround".char,a:type)
  endif
endfunction

function! s:opfunc2(arg)
  call s:opfunc(a:arg,1)
endfunction " }}}1

function! s:closematch(str) " {{{1
  " Close an open (, {, [, or < on the command line.
  let tail = matchstr(a:str,'.[^\[\](){}<>]*$')
  if tail =~ '^\[.\+'
    return "]"
  elseif tail =~ '^(.\+'
    return ")"
  elseif tail =~ '^{.\+'
    return "}"
  elseif tail =~ '^<.+'
    return ">"
  else
    return ""
  endif
endfunction " }}}1

nnoremap <silent> <Plug>Dsurround  :<C-U>call <SID>dosurround(<SID>inputtarget())<CR>
nnoremap <silent> <Plug>Csurround  :<C-U>call <SID>changesurround()<CR>
nnoremap <silent> <Plug>Yssurround :<C-U>call <SID>opfunc(v:count1)<CR>
nnoremap <silent> <Plug>Ygssurround :<C-U>call <SID>opfunc2(v:count1)<CR>
" <C-U> discards the numerical argument but there's not much we can do with it
nnoremap <silent> <Plug>Ysurround  :<C-U>set opfunc=<SID>opfunc<CR>g@
nnoremap <silent> <Plug>Ygsurround :<C-U>set opfunc=<SID>opfunc2<CR>g@
vnoremap <silent> <Plug>Vsurround  :<C-U>call <SID>opfunc(visualmode())<CR>
vnoremap <silent> <Plug>VSurround  :<C-U>call <SID>opfunc('V')<CR>
vnoremap <silent> <Plug>Vgsurround :<C-U>call <SID>opfunc2(visualmode())<CR>
vnoremap <silent> <Plug>VgSurround :<C-U>call <SID>opfunc2('V')<CR>
vnoremap <silent> <Plug>oVSurround  :<C-U>call <SID>opfunc(visualmode(),visualmode() ==# 'V' ? 1 : 0)<CR>
vnoremap <silent> <Plug>oVgSurround :<C-U>call <SID>opfunc(visualmode(),visualmode() ==# 'V' ? 0 : 1)<CR>
inoremap <silent> <Plug>Isurround  <C-R>=<SID>insert()<CR>
inoremap <silent> <Plug>ISurround  <C-R>=<SID>insert(1)<CR>

if !exists("g:surround_no_mappings") || ! g:surround_no_mappings
  if exists("g:surround_old_mappings") && g:surround_old_mappings
    nmap      ds   <Plug>Dsurround
    nmap      cs   <Plug>Csurround
    nmap      ys   <Plug>Ysurround
    nmap      yS   <Plug>Ygsurround
    nmap      yss  <Plug>Yssurround
    nmap      ySs  <Plug>Ygssurround
    nmap      ySS  <Plug>Ygssurround
    if !hasmapto("<Plug>Vsurround","v") && !hasmapto("<Plug>oVSurround","v")
      if exists(":xmap")
        xmap  s    <Plug>Vsurround
      else
        vmap  s    <Plug>Vsurround
      endif
    endif
    if !hasmapto("<Plug>oVSurround","v")
      if exists(":xmap")
        xmap  S    <Plug>oVSurround
      else
        vmap  S    <Plug>oVSurround
      endif
    endif
    if exists(":xmap")
      xmap    gS   <Plug>oVgSurround
    else
      vmap    gS   <Plug>oVgSurround
    endif
    if !hasmapto("<Plug>Isurround","i") && "" == mapcheck("<C-S>","i")
      imap    <C-S> <Plug>Isurround
    endif
    imap      <C-G>s <Plug>Isurround
    imap      <C-G>S <Plug>ISurround
    "Implemented internally instead
    "imap      <C-S><C-S> <Plug>ISurround
  else
    nmap      ds   <Plug>Dsurround
    nmap      cs   <Plug>Csurround
    nmap      ys   <Plug>Ysurround
    nmap      yS   <Plug>Ysurround$
    nmap      yss  <Plug>Yssurround
    nmap      ygs  <Plug>Ygsurround
    nmap      ygS  <Plug>Ygsurround$
    nmap      ygss <Plug>Ygssurround
    nmap      ygsgs <Plug>Ygssurround
    if !hasmapto("<Plug>Vsurround","v") && !hasmapto("<Plug>VSurround","v")
      if exists(":xmap")
        xmap  s    <Plug>Vsurround
      else
        vmap  s    <Plug>Vsurround
      endif
    endif
    if !hasmapto("<Plug>VSurround","v")
      if exists(":xmap")
        xmap  S    <Plug>VSurround
      else
        vmap  S    <Plug>VSurround
      endif
    endif
    if exists(":xmap")
      xmap    gs   <Plug>Vgsurround
    else
      vmap    gs   <Plug>Vgsurround
    endif
    if exists(":xmap")
      xmap    gS   <Plug>VgSurround
    else
      vmap    gS   <Plug>VgSurround
    endif
    if !hasmapto("<Plug>Isurround","i") && "" == mapcheck("<C-S>","i")
      imap    <C-S> <Plug>Isurround
    endif
    imap      <C-G>s <Plug>Isurround
    imap      <C-G>S <Plug>ISurround
    "Implemented internally instead
    "imap      <C-S><C-S> <Plug>ISurround
  endif
endif

let &cpo = s:cpo_save

" vim:set ft=vim sw=2 sts=2 et:
