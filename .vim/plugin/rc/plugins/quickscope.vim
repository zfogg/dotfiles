" quick-scope
scriptencoding utf-8


"let g:qs_enable = 0


"nnoremap <C-_> :normal ,<CR>
"vnoremap <C-_> :normal ,<CR>


"func! Quick_scope_selective(movement) abort
    "let needs_disabling = 0
    "if !g:qs_enable
        "QuickScopeToggle
        "redraw
        "let needs_disabling = 1
    "endif
    "let letter = nr2char(getchar())
    "if needs_disabling
        "QuickScopeToggle
    "endif
    "return a:movement . letter
"endfunc


"nnoremap <expr> <silent> f Quick_scope_selective('f')
"nnoremap <expr> <silent> F Quick_scope_selective('F')
"nnoremap <expr> <silent> t Quick_scope_selective('t')
"nnoremap <expr> <silent> T Quick_scope_selective('T')
"vnoremap <expr> <silent> f Quick_scope_selective('f')
"vnoremap <expr> <silent> F Quick_scope_selective('F')
"vnoremap <expr> <silent> t Quick_scope_selective('t')
"vnoremap <expr> <silent> T Quick_scope_selective('T')
