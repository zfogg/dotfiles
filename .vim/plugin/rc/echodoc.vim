" plugin/rc/echodoc
scriptencoding utf-8


if !has('nvim') | finish | endif


let g:echodoc#enable_at_startup = 1
let g:echodoc#type              = 'virtual'
let g:echodoc#events            = ['CompleteChanged']
"let g:echodoc#type              = 'floating'
" To use a custom highlight for the float window,
" change Pmenu to your highlight group
"highlight link EchoDocFloat Pmenu
