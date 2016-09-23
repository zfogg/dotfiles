" plugin/numberline


aug PluginConfig_numberline
    au!

    " in
    au    InsertEnter *
    \ setl relativenumber

    " out
    au    InsertLeave
        \,WinLeave
        \,BufLeave
        \,BufWinLeave
        \,TabLeave *
    \ setl norelativenumber
augroup END
