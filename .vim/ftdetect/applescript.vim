" ftdetect/applescript
scriptencoding utf-8


au BufRead,BufNewFile
        \ *.scpt
        \,*.scptd
        \,*.applescript
    \ setfiletype applescript
