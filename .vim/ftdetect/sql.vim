" ftdetect/sql
scriptencoding utf-8


let g:sql_type_default = 'pgsql'

au BufRead,BufNewFile
        \ *.pgsql,*.psql,*.psqlrc
    \ setfiletype sql
