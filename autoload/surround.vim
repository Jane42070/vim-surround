" surround.vim - Surroundings

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
