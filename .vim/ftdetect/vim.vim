" ftdetect/vim.vim


au BufRead,BufNewFile
        \ *.vim,.vimrc,_vimrc,vimrc
        \,*.nvim,.nvimrc,_nvimrc,nvimrc
        \,.exrc,.lvimrc
    \ setfiletype vim

