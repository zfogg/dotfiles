" deoplete
scriptencoding utf-8


if !has('nvim') | finish | endif


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

