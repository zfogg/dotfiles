" ftdetect/json.vim


au BufRead,BufNewFile 
        \ *.eslintrc*,*.babelrc*
    \ setfiletype json
