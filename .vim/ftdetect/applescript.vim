" ftdetect/applescript


au BufRead,BufNewFile
        \ *.scpt,*.scptd,*.applescript
    \ setfiletype applescript
