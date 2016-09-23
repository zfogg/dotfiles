" easyalign
scriptencoding utf-8


let g:easy_align_ignore_groups          = ['Comment', 'String']  "[]
let g:easy_align_interactive_modes      = ['l', 'r', 'c']        "['l', 'r', 'c']
let g:easy_align_bang_interactive_modes = ['r', 'l', 'c']        "['r', 'l', 'c']


nmap ga <Plug>(EasyAlign)
xmap ga <Plug>(EasyAlign)
