" surround.vim - Surroundings

let s:cpo_save = &cpo
set cpo&vim

" Surround object {{{1

let s:null = {'left': '', 'right': '', 'nspaces': 0, 'reindent': 0}
lockvar 2 s:null

function! surround#get(seq)
    let [spaces, reindent, key] = a:seq

    let sobj = surround#_get(key)
    if sobj == s:null
        return sobj
    endif

    let sobj['nspaces'] += len(spaces)

    if reindent == "\<C-t>"
        let sobj['reindent'] = 1
    elseif reindent == "\<C-d>"
        let sobj['reindent'] = 0
    endif

    return sobj
endfunction

function! surround#_get(key)
    if exists("b:surround_objects") && has_key(b:surround_objects, a:key)
        let raw_sobj = b:surround_objects[a:key]
    elseif has_key(g:surround_objects, a:key)
        let raw_sobj = g:surround_objects[a:key]
    elseif a:key !~ '\a'
        let raw_sobj = a:key . "\r" . a:key
    else
        return s:null
    endif

    if type(raw_sobj) == type({})
        return copy(raw_sobj)
    elseif type(raw_sobj) == type('')
        " Backward compatibility
        let raw_sobj = s:process(raw_sobj)
        return {'left': s:extractbefore(raw_sobj),
        \       'right': s:extractafter(raw_sobj),
        \       'nspaces': 0, 'reindent': 1}
    else
        return s:null
    endif
endfunction

function! surround#resolve(sobj)
    if has_key(a:sobj, 'inputfunc')
        let values = call(a:sobj['inputfunc'], get(a:sobj, 'inputfuncargs', []))
        if values == []
            return s:null
        endif

        let left = a:sobj['left']
        let right = a:sobj['right']

        for i in range(len(values))
            let left  = substitute(left,  nr2char(i + 1), values[i], 'g')
            let right = substitute(right, nr2char(i + 1), values[i], 'g')
        endfor

        let a:sobj['left'] = left
        let a:sobj['right'] = right
    endif

    return a:sobj
endfunction

function! surround#is_null(sobj)
    return a:sobj == s:null
endfunction

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

" }}}1

" Input functions {{{1

function! s:getchar()
    let c = getchar()
    if c =~ '^\d\+$'
        let c = nr2char(c)
    endif
    return c
endfunction

function! surround#inputtarget()
    let builtins = "wWsp[]()b<>t{}B\"'`"

    let cnt = ''
    let space = ''

    let c = s:getchar()

    if !g:surround_ignore_target_count
        while c =~ '\d'
            let cnt .= c
            let c = s:getchar()
        endwhile
    endif

    if c == " "
        let space = ' '
        let c = s:getchar()
    endif

    if c == "\<Esc>" || c == "\<C-c>"
        return []
    endif

    let target = c

    if strlen(target) == 1 && stridx(builtins, target) != -1 &&
    \  mapcheck('a' . target, 'o') == '' && mapcheck('i' . target, 'o') == ''
        return [cnt, space, target]
    endif

    while mapcheck('a' . target, 'o') != '' && maparg('a' . target, 'o') == ''
    \  && mapcheck('a' . target, 'o') != '' && maparg('i' . target, 'o') == ''
        let c  = s:getchar()
        if c  == "\<Esc>" || c == "\<C-c>"
            return []
        endif
        let target .= c
    endwhile

    return [cnt, space, target]
endfunction

function! surround#inputreplacement()
    let spaces = ''
    let reindent = ''

    while 1
        let c = s:getchar()
        if c == ' '
            let spaces .= c
        elseif c == "\<C-d>" || c == "\<C-t>"
            let reindent = c
        elseif c == "\<Esc>" || c == "\<C-c>"
            return []
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
            return []
        endif
        let replacement .= c
    endwhile

    return [spaces, reindent, replacement]
endfunction

function! surround#simple_input(...)
    let args = a:0 == 0 ? ['input: '] : a:000
    let values = []

    try
        for i in args
            if type(i) == type([])
                call add(values, input(i[0], i[1]))
            else
                call add(values, input(i))
            endif
        endfor
    catch
        return []
    endtry

    return values
endfunction

function! surround#simple_input1(...)
    let values = call('surround#simple_input', a:000)

    if len(values) == 1 && values[0] == ''
        return []
    else
        return values
    endif
endfunction

function! surround#tag_input()
    let values = surround#simple_input1('tag: ')
    if values == []
        return []
    endif

    call add(values, substitute(values[0], '\s.*', '', ''))
    return values
endfunction

function! surround#tag_input_lastdel()
    let last = exists("b:surround_lastdel") ? b:surround_lastdel : ''
    let default = matchstr(last, '<\zs.\{-\}\ze>')

    let values = surround#simple_input(['tag: ', default])
    if values == []
        return []
    endif

    call add(values, substitute(values[0], '\s.*', '', ''))
    return values
endfunction

function! surround#latex_input()
    let pairs = "[](){}<>"

    let values = surround#simple_input1('\begin{')
    if values == []
        return []
    endif

    let env = '{' . values[0]
    let tail = matchstr(env, '\v.[^\[\]()){}}<>]+$')
    if tail != ""
        let env .= pairs[stridx(pairs, tail[0]) + 1]
    endif
    return [env, substitute(env, '[^}]*$', '', '')]
endfunction

" }}}1

" Wrapping functions {{{1

function! surround#wrap(sobj, inner, type, special, indent)
    let left = a:sobj['left']
    let right = a:sobj['right']
    let spaces = repeat(' ', a:sobj['nspaces'])

    if a:type =~ "^\<C-V>"
        let lines = split(a:inner, "\n")
        call map(lines, "left . spaces . v:val . spaces . right")
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
                " Ignore the newline at the beginning of 'left' because it
                " already exists.  This also means "don't indent 'left'."
                let left = strpart(left, 1)
            endif
        else
            if a:type ==# 'V'
                " Indent 'left' with the same level of first line.
                let left = a:indent . left
            endif
        endif

        "
        " Fixup the end of the 'left'
        "
        if left !~ '\n$'
            " A newlines is required before the content in special mode.
            let left = substitute(left,' \+$','','') . "\n"
        endif
        if a:type ==# 'v'
            " Indent the content.
            let left = left . a:indent
        endif

        "
        " Fixup the beginning of the 'right'
        "
        if right =~ '^\n'
            if a:type ==# 'V'
                " Ignore the newline at the beginning of the 'right'.  because
                " it already exists.  This also means "don't indent 'right'."
                let right = strpart(right, 1)
            endif
        else
            " Indent the 'right' at the same level of first line.
            let right  = a:indent . substitute(right ,'^ \+','','')
            if a:type ==# 'v'
                " A newline is required after the content in charwise mode.
                let right = "\n" . right
            endif
        endif

        "
        " Fixup the end of the 'right'
        "
        if right !~ '\n$'
            if a:type ==# 'V'
                " Add a newline at the end of the 'right'.
                let right = right . "\n"
            endif
        endif
    endif

    return left . a:inner . right
endfunction

function! surround#wrapreg(sobj, reg, ...)
    let orig = getreg(a:reg)
    let type = getregtype(a:reg)
    let special = a:0 > 0 ? a:1 : 0
    let indent = a:0 > 1 ? a:2 : ''
    let new = surround#wrap(a:sobj, orig, type, special, indent)
    call setreg(a:reg, new, type)
endfunction

" }}}1

" Insert mode functions {{{1

function! surround#insert(...)
    let special = a:0 ? a:1 : 0

    let rseq = surround#inputreplacement()
    while rseq[2] == "\<CR>" || rseq[2] == "\<C-S>"
        let special = !special
        let rseq = surround#inputreplacement()
    endwhile

    if rseq == []
        return ""
    endif

    let cb_save = &clipboard
    set clipboard-=unnamed clipboard-=unnamedplus
    let reg_save = [getreg('a'), getregtype('a')]

    let sobj = surround#resolve(surround#get(rseq))

    let new = surround#wrap(sobj, "\r", "v", special, '')

    if exists("g:surround_insert_tail")
        let new .= g:surround_insert_tail
    endif

    call setreg('a', new, 'v')

    if col('.') >= col('$')
        normal! "ap
    else
        normal! "aP
    endif

    if special
        call s:reindent(sobj)
    endif

    norm! `]
    call search('\r', 'bW')

    call setreg('a', reg_save[0], reg_save[1])
    let &clipboard = cb_save

    return "\<Del>"
endfunction

" }}}1

" Normal and Visual mode functions {{{1

function! surround#dosurround(...)
    let tseq = a:0 > 0 ? a:1 : surround#inputtarget()
    let rseq = a:0 > 1 ? a:2 : []

    if tseq == []
        return
    endif

    let tcount = v:count1 * (tseq[0] == "" ? 1 : tseq[0])
    let tspace = len(tseq[1])
    let target = tseq[2]

    if target == ''
        return
    endif

    let pos_save = getpos(".")
    let reg_save = [getreg('a'), getregtype('a')]

    call setreg('a', '')
    " Don't use normal! for user-defined objects.
    execute 'silent normal "ay' . tcount . 'a' . target
    if getreg('a') == ''
        " No text object found.
        call setreg('a', reg_save[0], reg_save[1])
        call setpos(".", pos_save)
        return
    endif
    let whole_pos = [getpos("'["), getpos("']")]

    call setpos(".", pos_save)

    call setreg('a', '')
    " Don't use normal! for user-defined objects.
    execute 'silent normal "ay' . tcount . 'i' . target
    if getreg('a') == ''
        " No inner object found.
        call setreg('a', reg_save[0], reg_save[1])
        call setpos(".", pos_save)
        return
    endif
    let inner = getreg('a')
    let innertype = getregtype('a')

    if rseq != []
        let sobj = surround#resolve(surround#get(rseq))
        if surround#is_null(sobj)
            call setreg('a', reg_save[0], reg_save[1])
            call setpos(".", pos_save)
            return
        endif
    endif

    let sel_save = &selection
    let &selection = "inclusive"

    call setpos(".", pos_save)
    let before = ''
    let after = ''

    if target =~# '^[Wpsw]$'
        " For non-block objects, determine inner object from a whole object.
        " Deference in a while object and inner object of these does not
        " represent surrounding.
        "
        " example)
        " thi*s is example  ; * is cursor, here
        "   2daw -> example (deleted "this is ")
        "   2diw -> is example (deleted "this ")
        " Here, we prefer to wrap 2 words with 2csw command.
        execute 'silent normal "ad' . tcount . 'a' . target

        let whole = getreg('a')
        let wholetype = getregtype('a')

        let mlist = matchlist(whole, '^\v(\_s*)(.{-})(\_s*)$')
        let before = mlist[1]
        let inner = mlist[2]
        let after = mlist[3]
        let surrounds = ['', '']
    else
        " For block objects, we assume that difference in a whole object and
        " inner object is surrounding object.

        " Don't use normal! for user-defined objects
        execute 'silent normal "ad' . tcount . 'a' . target

        if tspace && innertype ==# 'V' && getregtype('a') ==# 'v'
            " Delete white spaces before and after the surrounding.
            undo
            call setpos('.', whole_pos[0])
            call search('\%(^\|\S\)\zs\s*\%#')
            normal! m[
            call setpos('.', whole_pos[1])
            call search('\%#.\s*\ze\%($\|\S\)', 'e')
            let delete_space = (col('.') + 1 == col('$'))
            normal! v`["ad
        endif

        let whole = getreg('a')
        let wholetype = getregtype('a')

        let idx = (inner == '') ? 1 : stridx(whole, inner)
        let len = strlen(inner)
        let surrounds = [strpart(whole, 0, idx), strpart(whole, idx + len)]

        if tspace
            if innertype ==# 'V'
                if wholetype ==# 'v'
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

    if rseq != []
        let new = surround#wrap(sobj, inner, 'v', 0, '')
    else
        let new = inner
    endif
    call setreg('a', before . new . after, wholetype)

    undojoin
    execute 'silent normal! "a' . s:getpcmd(wholetype)

    let ww_save = &whichwrap
    let &whichwrap = "<,>"

    if before != ''
        normal! `[
        execute 'normal! ' . strlen(before) . "\<Right>"
        normal! m[
    endif

    if after != ''
        normal! `]
        execute 'normal! ' . strlen(after) . "\<Left>"
        normal! m]
    endif

    let &whichwrap = ww_save

    normal! `[

    let b:surround_lastdel = join(surrounds, '')
    call setreg('"', b:surround_lastdel)

    let &selection = sel_save
    call setreg('a', reg_save[0], reg_save[1])

    if rseq == []
        silent! call repeat#set("\<Plug>Dsurround" . join(tseq, ''), tcount)
    else
        silent! call repeat#set("\<Plug>Csurround" . join(tseq, '') .
        \                       join(rseq, ''), tcount)
    endif
endfunction

function! surround#changesurround()
    let tseq = surround#inputtarget()
    if tseq == []
        return ""
    endif
    let rseq = surround#inputreplacement()
    if rseq == []
        return ""
    endif
    call surround#dosurround(tseq, rseq)
endfunction

let s:type_map = {"char": "v", "line": "V", "block": "\<C-v>"}

function! surround#opfunc(type, ...)
    let special = a:0 > 0 ? a:1 : 0

    let rseq = surround#inputreplacement()
    if rseq == []
        return
    endif

    let sobj = surround#resolve(surround#get(rseq))
    if surround#is_null(sobj)
        return
    endif

    let reg_save = [getreg('a'), getregtype('a')]
    let sel_save = &selection
    let cb_save  = &clipboard

    let &selection = "inclusive"
    set clipboard-=unnamed clipboard-=unnamedplus

    if has_key(s:type_map, a:type)
        let indent = matchstr(getline("'["), '^\s*')
        execute 'silent normal! `[' . s:type_map[a:type] . '`]"ad'
    elseif index(["v", "V", "\<C-v>"], a:type) >= 0
        let indent = matchstr(getline("'<"), '^\s*')
        let ve_save = &virtualedit
        if !special
            let &virtualedit = ''
        endif
        execute 'silent normal! `<' . a:type . '`>"ad'
        let &virtualedit = ve_save
    elseif a:type =~ '^\d\+$'
        let indent = matchstr(getline("."), '^\s*')
        execute 'silent normal! "a' . a:type . 'dd'
    else
        let &selection = sel_save
        let &clipboard = cb_save
        return
    endif

    let type = getregtype('a')
    let otype = type
    let inner = getreg('a')
    let before = ''
    let after = ''

    if type ==# 'v' || (type ==# 'V' && !special)
        let type = 'v'
        let mlist = matchlist(inner, '^\(\s*\)\(.\{-\}\)\(\n\?\s*\)$')
        let inner = mlist[2]
        let before = mlist[1]
        let after = mlist[3]
    endif

    let new = surround#wrap(sobj, inner, type, special, indent)

    call setreg('a', before . new . after, otype)
    silent execute 'normal! "a' . s:getpcmd(otype) . '`['

    if special
        if type ==# 'V' && sobj['nspaces'] > 0
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

        call s:reindent(sobj)
    endif

    let &clipboard = cb_save
    let &selection = sel_save
    call setreg('a', reg_save[0], reg_save[1])

    if a:type =~ '^\d\+$'
        silent! call repeat#set(
        \   "\<Plug>Y" . (special ? "gs" : "s") . "surround" . join(rseq, ''),
        \   a:type)
    else
        silent! call repeat#set("\<Plug>SurroundRepeat" . join(rseq, ''))
    endif
endfunction

function! surround#opfunc_s(arg)
    call surround#opfunc(a:arg, 1)
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

let &cpo = s:cpo_save
