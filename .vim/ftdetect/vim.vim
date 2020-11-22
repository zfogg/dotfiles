" ftdetect/vim
scriptencoding utf-8


au BufRead,BufNewFile
        \ *.vim,.vimrc,_vimrc,vimrc
        \,*.nvim,.nvimrc,_nvimrc,nvimrc
        \,*.exrc,.exrc,_exrc,exrc
        \,*.lvimrc,.lvimrc,_lvimrc,lvimrc
    \ setfiletype vim
