" ftdetect/sql


let g:sql_type_default = 'pgsql'

au BufRead,BufNewFile
        \ *.pgsql,*.psql,*.psqlrc
    \ setfiletype sql
