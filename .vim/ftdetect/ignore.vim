" ftdetect/ignore
scriptencoding utf-8


au BufRead,BufNewFile
        \ .ignore,ignore
        \,.gitignore,gitignore
        \,.rgignore,rgignore
        \,.fdignore,fdignore
    \ setfiletype gitignore
