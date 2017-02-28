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

let g:deoplete#sources#clang#libclang_path = s:libclang_path
let g:deoplete#sources#clang#clang_header  = s:clang_header

let g:deoplete#enable_at_startup = 1  " just work
let g:deoplete#enable_smart_case = 1  " smartcase
let g:deoplete#max_list          = 65 " default=100


" :help 'deoplete-filters' {{{
call deoplete#custom#set('_', 'matchers', [
        \ 'matcher_head',
        \ 'matcher_length',
        \ 'matcher_fuzzy',
    \ ])

call deoplete#custom#set('_', 'sorters', [
        \ 'sorter_rank',
    \ ])

call deoplete#custom#set('_', 'converters', [
        \'converter_remove_overlap',
        \'converter_truncate_abbr',
        \'converter_truncate_menu',
        \'converter_auto_paren',
        \'converter_auto_delimiter',
    \])

"call deoplete#custom#set('_',
    "\ 'disabled_syntaxes', ['Comment', 'String'])
" :help 'deoplete-filters' }}}


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
        "\'syntaxcomplete#Complete', ]

let s:sources.javascript = ['ternjs'] + s:sources._
let s:omnifns.javascript = [
    \'tern#Complete','jspc#omni',
    \'javascriptcomplete#CompleteJS'] + s:omnifns._
"let s:sources['javascript.jsx'] = s:sources.javascript
"let s:omnifns['javascript.jsx'] = s:omnifns.javascript

let s:sources.python = ['jedi']                    + s:sources._
let s:omnifns.python = ['pythonComplete#Complete'] + s:omnifns._
"let s:sources.python3 = s:sources.python
"let s:omnifns.python3 = s:omnifns.python

let s:sources.vim = ['vim'] + s:sources._
let s:omnifns.vim = [     ] + s:omnifns._

let s:sources.rust = ['racer']               + s:sources._
let s:omnifns.rust = ['racer#RacerComplete'] + s:omnifns._

" NOTE: export at the bottom
let g:deoplete#sources        = s:sources
let g:deoplete#omni#functions = s:omnifns
" }}}


aug RcPlugin__deoplete
    au!
	au CompleteDone * pclose!
aug END


" python
let g:deoplete#sources#jedi#statement_length = 75
let g:deoplete#sources#jedi#enable_cache     = 1
let g:deoplete#sources#jedi#show_docstring   = 0
" NOTE: how to debug
"let g:deoplete#enable_profile = 0
"call deoplete#custom#set('jedi', 'debug_enabled', 0)
"call deoplete#enable_logging('DEBUG', '/tmp/deoplete.log')

" go
let g:deoplete#sources#go#gocode_binary  = exepath('gocode')
let g:deoplete#sources#go#sort_class     = ['package', 'func', 'type', 'var', 'const']
let g:deoplete#sources#go#use_cache      = 1
let g:deoplete#sources#go#json_directory = $HOME.'/tmp'
let g:deoplete#sources#go#cgo#libclang_path = s:libclang_path
