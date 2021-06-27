" after/plugin/deoplete
scriptencoding utf-8


if !has('nvim') ||
      \ !z#util#HasPlugin('deoplete.nvim') ||
      \ !get(g:, 'deoplete#enable_at_startup', v:false)
  finish | endif


" deoplete#{sources,omni#functions} {{{
" NOTE: import at the top
"let s:sources = get(g:, 'deoplete#sources',        {})
"let s:omnifns = get(g:, 'deoplete#omni#functions', {})

"let s:sources._ = [
      "\'omni',
      "\'member',
      "\'around',
      "\'syntax',
      "\'buffer',
      "\'file/include',
  "\]
"let s:omnifns._ = [ ]

" javascript
"for s in z#constants#globals#Ft().js
  "let s:sources[s] = ['ternjs'] + s:sources._
  "let s:omnifns[s] = ['ternjs'] + [
        "\'tern#Complete','jspc#omni',
        "\'javascriptcomplete#CompleteJS'] + s:omnifns._
"endfor

" python
for p in z#constants#globals#Ft().py
  let s:sources[p] = ['jedi']                    + s:sources._
  let s:omnifns[p] = ['pythonComplete#Complete'] + s:omnifns._
endfor
let g:deoplete#sources#jedi#statement_length = 75
let g:deoplete#sources#jedi#enable_cache     = 1
let g:deoplete#sources#jedi#show_docstring   = 1
NOTE: how to debug
let g:deoplete#enable_profile = 0
call deoplete#custom#source('jedi', 'debug_enabled', 0)
call deoplete#enable_logging('DEBUG', '/tmp/deoplete.log')

" vim
"for v in z#constants#globals#Ft().vim
  "let s:sources[v] = ['vim'] + s:sources._
  "let s:omnifns[v] = [     ] + s:omnifns._
"endfor

" rust
"for r in z#constants#globals#Ft().rs
  "let s:sources[r] = ['racer']               + s:sources._
  "let s:omnifns[r] = ['racer#RacerComplete'] + s:omnifns._
"endfor

" php
"for r in z#constants#globals#Ft().php
"endfor
"let g:deoplete#ignore_sources     = get(g:, 'deoplete#ignore_sources', {})
"call g:deoplete#custom#option('deoplete#ignore_sources', {})
"let g:deoplete#ignore_sources.php = ['omni']

" golang
"let g:deoplete#sources#go#gocode_binary  = exepath('gocode')
"let g:deoplete#sources#go#sort_class     = ['package', 'func', 'type', 'var', 'const']
"let g:deoplete#sources#go#use_cache      = 1
"let g:deoplete#sources#go#json_directory = $HOME.'/tmp'

" emoji
"if has('mac') && z#util#HasPlugin('deoplete-emoji')
  "call deoplete#custom#source('emoji', 'filetypes', ['rst'])
"endif

" neosnippet
"if z#util#HasPlugin('neosnippet')
  "call add(s:sources, ['neosnippet'])
"endif

" NOTE: export to global at the BOTTOM
"let g:deoplete#sources        = s:sources
"let g:deoplete#omni#functions = s:omnifns
" }}}
