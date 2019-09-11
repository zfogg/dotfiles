" ftplugin/rust
scriptencoding utf-8


let b:neomake_open_list=0

setl makeprg=cargo\ $*
compiler cargo

setl hidden

nnoremap <buffer> gd    <Plug>(rust-def)
nnoremap <buffer> gs    <Plug>(rust-def-split)
nnoremap <buffer> gx    <Plug>(rust-def-vertical)
nnoremap <buffer> <S-k> <Plug>(rust-doc)
