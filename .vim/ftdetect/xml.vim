" ftdetect/xml


au BufRead,BufNewFile
        \ *.plist
        \,*.xml
    \ setfiletype xml
