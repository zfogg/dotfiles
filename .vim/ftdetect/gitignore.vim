" ftdetect/gitignore


au BufRead,BufNewFile
        \ .gitignore,.gitignore_global
        \,.rgignore
        \,.agignore
    \ setfiletype gitignore
