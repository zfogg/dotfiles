" ftdetect/gitignore
scriptencoding utf-8


au BufRead,BufNewFile
        \ .gitignore,.gitignore_global
        \,.rgignore
        \,.agignore
    \ setfiletype gitignore
