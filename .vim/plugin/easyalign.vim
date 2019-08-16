" easyalign
scriptencoding utf-8


let g:easy_align_ignore_groups          = ['Comment', 'String']  "[]
let g:easy_align_interactive_modes      = ['l', 'r', 'c']        "['l', 'r', 'c']
let g:easy_align_bang_interactive_modes = ['r', 'l', 'c']        "['r', 'l', 'c']


" NOTE: requires tpope/vim-repeat
nnoremap ga <Plug>(EasyAlignRepeat)
xnoremap ga <Plug>(EasyAlignRepeat)

nnoremap gla <Plug>(LiveEasyAlignRepeat)
xnoremap gla <Plug>(LiveEasyAlignRepeat)
