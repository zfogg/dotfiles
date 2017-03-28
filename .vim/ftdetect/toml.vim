" ftdetect/toml
scriptencoding utf-8


au BufRead,BufNewFile
        \ Pipfile
    \ setfiletype toml
