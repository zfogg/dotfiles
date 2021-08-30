" plugin/numberline
scriptencoding utf-8
"   requires:
"       autoload/numberline


" FIXME: do i event want this?
if v:true | finish | endif


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

    let s:myvimrc = escape(expand($MYVIMRC), '\')
    exe 'au SourceCmd '.s:myvimrc.' call numberline#On()'

    " except
    au    BufEnter
        \,BufLeave
        \,BufNew
        \,BufAdd
            \ NERD_tree_*
        \ setl number<
augroup END
