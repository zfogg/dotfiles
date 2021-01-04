" ftdetect/brewfile
scriptencoding utf-8


au BufRead,BufNewFile
        \ .Brewfile
        \,Brewfile
        \,*.Brewfile
    \ setfiletype ruby
