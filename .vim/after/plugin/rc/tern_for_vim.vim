" tern_for_vim
scriptencoding utf-8


let g:tern#command   = ['tern']
let g:tern#arguments = ['--persistent']

" Use deoplete.
let g:tern_request_timeout       = 1
" This do disable full signature type on autocomplete
let g:tern_show_signature_in_pum = '0'
