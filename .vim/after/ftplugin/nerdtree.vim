" ftplugin/nerdtree
scriptencoding utf-8


let b:noNERDTreeAutoCWD=1

aug aug_ft_nerdtree
    au!
    au    BufEnter
        \,BufLeave
        \,BufNew
        \,BufAdd
            \ NERD_tree_*
        \ setl nocul nocuc nonu
aug END
