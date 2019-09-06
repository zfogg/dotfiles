" after/plugin/gitgutter
scriptencoding utf-8


nnoremap <silent> <expr> <Plug>GitGutterNextHunk &diff ? ']c' : ":<C-U>execute v:count1 . 'GitGutterNextHunk'<CR>"
nnoremap <silent> <expr> <Plug>GitGutterPrevHunk &diff ? '[c' : ":<C-U>execute v:count1 . 'GitGutterPrevHunk'<CR>"
