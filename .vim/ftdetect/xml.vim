" ftdetect/xml


"au BufRead,BufNewFile
        "\ *.plist
        "\,*.xml
    "\ setfiletype xml

au BufRead,BufNewFile
        \ *.xml
    \ setfiletype xml
