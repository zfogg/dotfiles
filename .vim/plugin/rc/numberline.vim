" plugin/numberline
scriptencoding utf-8
"   requires:
"       autoload/numberline


augroup PluginConfig_numberline
    autocmd!

    " no-op
    au  FileType
            \ help
            \,nerdtree
        \ let b:nonumberline=1

    " out
    au    WinLeave
        \,TabLeave
        \,BufLeave
        \,BufWinLeave
            \ *
        \ setl number<

    " in
    au    WinEnter
        \,TabEnter
        \,BufEnter
        \,BufWinEnter
            \ *
        \ call numberline#On()
    exe 'au SourceCmd '.$MYVIMRC.' call numberline#On()'

    " except
    au    BufEnter
        \,BufLeave
        \,BufNew
        \,BufAdd
            \ NERD_tree_*
        \ setl number<
augroup END
