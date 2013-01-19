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
nnoremap <silent> <Plug>Dsurround   :<C-u>call surround#dosurround()<CR>
nnoremap <silent> <Plug>Csurround   :<C-u>call surround#changesurround()<CR>
nnoremap <silent> <Plug>Yssurround  :<C-u>call surround#opfunc(v:count1)<CR>
nnoremap <silent> <Plug>Ygssurround :<C-u>call surround#opfunc_s(v:count1)<CR>
nnoremap <silent> <Plug>Ysurround   :<C-u>set opfunc=surround#opfunc<CR>g@
nnoremap <silent> <Plug>Ygsurround  :<C-u>set opfunc=surround#opfunc_s<CR>g@
vnoremap <silent> <Plug>Vsurround   :<C-u>call surround#opfunc(visualmode())<CR>
vnoremap <silent> <Plug>VSurround   :<C-u>call surround#opfunc('V')<CR>
vnoremap <silent> <Plug>Vgsurround  :<C-u>call surround#opfunc_s(visualmode())<CR>
vnoremap <silent> <Plug>VgSurround  :<C-u>call surround#opfunc_s('V')<CR>
vnoremap <silent> <Plug>oVSurround  :<C-u>call surround#opfunc(visualmode(), visualmode() ==# 'V' ? 1 : 0)<CR>
vnoremap <silent> <Plug>oVgSurround :<C-u>call surround#opfunc(visualmode(), visualmode() ==# 'V' ? 0 : 1)<CR>
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
