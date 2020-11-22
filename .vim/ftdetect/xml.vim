" ftdetect/xml
scriptencoding utf-8


"au BufRead,BufNewFile
        "\ *.plist
        "\,*.xml
    "\ setfiletype xml

au BufRead,BufNewFile
        \ *.xml
    \ setfiletype xml
