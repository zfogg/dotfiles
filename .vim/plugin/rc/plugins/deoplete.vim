" deoplete
scriptencoding utf-8


if exists('g:vimpager') | finish | endif


let s:llvm_confs = {
    \ 'mac': {
        \ 'libclang_glob': 'libclang.{so,dylib}'
        \,'lib_dir': {
            \ 'system': '/Library/Developer/CommandLineTools/usr/lib'
            \,'custom': '/usr/local/opt/llvm/lib'
        \}
    \}
    \,'unix': {
        \ 'libclang_glob': 'libclang.{so,so.1}'
        \,'lib_dir': {
            \ 'system' : '/usr/lib'
        \},
    \}
\}

let s:llvm_conf = s:llvm_confs[has('mac') ? 'mac' : 'unix']

let s:llvm_lib = get(s:llvm_conf.lib_dir, 'custom',
                \get(s:llvm_conf.lib_dir, 'system'))

let s:libclang_path = globpath(s:llvm_lib, s:llvm_conf.libclang_glob, 1, 0)
let s:clang_header  = globpath(s:llvm_lib, 'clang',                   1, 0)

let g:deoplete#sources#clang#libclang_path = s:libclang_path
let g:deoplete#sources#clang#clang_header  = s:clang_header

let g:deoplete#enable_at_startup = 1 " just work
let g:deoplete#enable_smart_case = 1 " smartcase


call deoplete#custom#set('_', 'matchers', ['matcher_fuzzy'])


" deoplete#{sources,omni#functions} {{{
" NOTE: import at the top
let s:sources = get(g:, 'deoplete#sources',        {})
let s:omnifns = get(g:, 'deoplete#omni#functions', {})

let s:sources.javascript = [
        \'buffer',
        \'member',
        \'file',
        \'omni',
    \]
let s:omnifns.javascript = [
        \'tern#Complete',
        \'jspc#omni',
    \]

let s:sources['javascript.jsx'] = get(s:sources, 'javascript', [])
let s:omnifns['javascript.jsx'] = get(s:omnifns, 'javascript', [])

" NOTE: export at the bottom
let g:deoplete#omni#functions = s:omnifns
let g:deoplete#sources        = s:sources
" }}}


aug RcPlugin__deoplete
    au!
	au CompleteDone * pclose!
aug END
