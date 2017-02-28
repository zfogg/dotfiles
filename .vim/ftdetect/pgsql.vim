" ftdetect/pgsql


au BufRead,BufNewFile
        \ *.pgsql
    \ setfiletype sql
