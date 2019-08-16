" deoplete
scriptencoding utf-8


if !has('nvim')         | finish | endif
if exists('g:vimpager') | finish | endif


let g:deoplete#max_menu_width = 85


let s:llvm_confs = {
    \ 'mac': {
        \ 'libclang_glob': 'libclang.{so,dylib}'
        \,'lib_dir': {
            \ 'system': '/Library/Developer/CommandLineTools/usr/lib'
            \,'custom': '/usr/local/opt/llvm/lib'
        \}, }
    \,'unix': {
        \ 'libclang_glob': 'libclang.{so,so.1}'
        \,'lib_dir': {
            \ 'system' : '/usr/lib'
        \}, }, }

let s:llvm_conf = s:llvm_confs[has('mac') ? 'mac' : 'unix']

let s:llvm_lib = get(s:llvm_conf.lib_dir, 'custom',
                \get(s:llvm_conf.lib_dir, 'system'))

let s:libclang_path = globpath(s:llvm_lib, s:llvm_conf.libclang_glob, 1, 0)
let s:clang_header  = globpath(s:llvm_lib, 'clang',                   1, 0)

let g:deoplete#enable_at_startup = 1  " just work
let g:deoplete#enable_smart_case = 1  " smartcase
let g:deoplete#max_list          = 65 " default=100


" :help 'deoplete-filters' {{{
call deoplete#custom#source('_', 'matchers', [
        \ 'matcher_head',
        \ 'matcher_length',
        \ 'matcher_fuzzy',
    \ ])

call deoplete#custom#source('_', 'sorters', [
        \ 'sorter_rank',
    \ ])

call deoplete#custom#source('_', 'converters', [
        \'converter_remove_overlap',
        \'converter_truncate_abbr',
        \'converter_truncate_menu',
        \'converter_auto_paren',
        \'converter_auto_delimiter',
    \])

call deoplete#custom#source('_',
    \ 'disabled_syntaxes', ['Comment'])
" :help 'deoplete-filters' }}}

call deoplete#custom#source('_', 'libclang_path', s:libclang_path)
call deoplete#custom#source('_', 'clang_header',  s:clang_header)

" deoplete#{sources,omni#functions} {{{
" NOTE: import at the top
let s:sources = get(g:, 'deoplete#sources',        {})
let s:omnifns = get(g:, 'deoplete#omni#functions', {})

let s:sources._ = [
        \'omni',
        \'member',
        \'around',
        \'syntax',
        \'buffer',
        \'file/include',
        \'neosnippet', ]
let s:omnifns._ = [ ]

" javascript
for s in z#constants#globals#Ft().js
  let s:sources[s] = ['ternjs'] + s:sources._
  let s:omnifns[s] = ['ternjs'] + [
        \'tern#Complete','jspc#omni',
        \'javascriptcomplete#CompleteJS'] + s:omnifns._
endfor

" python
for p in z#constants#globals#Ft().py
  let s:sources[p] = ['jedi']                    + s:sources._
  let s:omnifns[p] = ['pythonComplete#Complete'] + s:omnifns._
endfor
let g:deoplete#sources#jedi#statement_length = 75
let g:deoplete#sources#jedi#enable_cache     = 1
let g:deoplete#sources#jedi#show_docstring   = 1
" NOTE: how to debug
"let g:deoplete#enable_profile = 0
"call deoplete#custom#source('jedi', 'debug_enabled', 0)
"call deoplete#enable_logging('DEBUG', '/tmp/deoplete.log')

" vim
for v in z#constants#globals#Ft().vim
  let s:sources[v] = ['vim'] + s:sources._
  let s:omnifns[v] = [     ] + s:omnifns._
endfor

" rust
for r in z#constants#globals#Ft().rs
  let s:sources[r] = ['racer']               + s:sources._
  let s:omnifns[r] = ['racer#RacerComplete'] + s:omnifns._
endfor

" php
"for r in z#constants#globals#Ft().php
"endfor
let g:deoplete#ignore_sources = get(g:, 'deoplete#ignore_sources', {})
let g:deoplete#ignore_sources.php = ['omni']


" golang
let g:deoplete#sources#go#gocode_binary  = exepath('gocode')
let g:deoplete#sources#go#sort_class     = ['package', 'func', 'type', 'var', 'const']
let g:deoplete#sources#go#use_cache      = 1
let g:deoplete#sources#go#json_directory = $HOME.'/tmp'


" emoji
call deoplete#custom#source('emoji', 'filetypes', ['rst'])


" NOTE: export to global at the BOTTOM
let g:deoplete#sources        = s:sources
let g:deoplete#omni#functions = s:omnifns
" }}}


aug RcPlugin__deoplete
    au!
    au CompleteDone * pclose!
    au VimEnter     * call deoplete#initialize()
aug END


" deoplete-padawan
command! PadawanStart call deoplete#sources#padawan#StartServer()
command! PadawanStop call deoplete#sources#padawan#StopServer()
command! PadawanRestart call deoplete#sources#padawan#RestartServer()
command! PadawanInstall call deoplete#sources#padawan#InstallServer()
command! PadawanUpdate call deoplete#sources#padawan#UpdatePadawan()
command! -bang PadawanGenerate call deoplete#sources#padawan#Generate(<bang>0)
