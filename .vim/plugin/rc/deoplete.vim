" plugin/rc/deoplete
scriptencoding utf-8


if !has('nvim') | finish | endif


let g:deoplete#enable_at_startup = 1  " just work

call deoplete#custom#option('smart_case', v:true)
call deoplete#custom#option('max_list', 65)
call deoplete#custom#option('min_pattern_length', 1)

call deoplete#custom#source('_', 'max_menu_width', 93)

"let g:deoplete#enable_smart_case = 1  " smartcase
"let g:deoplete#max_list          = 65 " default=100
"let g:deoplete#max_menu_width = 85
