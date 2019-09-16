" ftplugin/rust
scriptencoding utf-8


let b:neomake_open_list=0

setl makeprg=cargo\ $*
compiler cargo

setl hidden

"nnoremap <buffer> gd    <Plug>(rust-def)
"nnoremap <buffer> gs    <Plug>(rust-def-split)
"nnoremap <buffer> gx    <Plug>(rust-def-vertical)
"nnoremap <buffer> <S-k> <Plug>(rust-doc)

nnoremap <buffer> <silent> K    :call LanguageClient_textDocument_hover()<CR>
nnoremap <buffer> <silent> gd   :call LanguageClient_textDocument_definition()<CR>
"nnoremap <buffer> <silent> <F2> :call LanguageClient_textDocument_rename()


aug Rust_LanguageClient_AutoDef
    au! CursorHold <buffer>
    au CursorHold <buffer> :normal K<CR>
aug END
