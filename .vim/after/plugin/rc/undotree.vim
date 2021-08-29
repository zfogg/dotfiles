" plugin/rc/undotree
scriptencoding utf-8

if !PHas('undotree') | finish | endif

let g:undotree_HighlightChangedWithSign = 1
let g:undotree_WindowLayout             = 3
let g:undotree_SetFocusWhenToggle       = 0
let g:undotree_DiffAutoOpen             = 1

nnoremap <Leader>u :UndotreeToggle<CR>
