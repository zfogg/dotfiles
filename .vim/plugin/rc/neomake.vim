" plugin/rc/neomake
scriptencoding utf-8


let g:neomake_open_list              = 0
let g:neomake_list_height            = 5
let g:neomake_verbose                = 0
let g:neomake_airline                = 0
let g:neomake_remove_invalid_entries = 1
let g:neomake_place_signs            = 1
let g:neomake_highlight_columns      = 1
let g:neomake_highlight_lines        = 1


hi NeomakeErrorSign   term=bold gui=bold ctermfg=red    guifg=red
hi NeomakeWarningSign term=bold gui=bold ctermfg=yellow guifg=yellow

