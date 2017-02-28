" ftdetect/json


au BufRead,BufNewFile 
        \ *.eslintrc*,*.babelrc*
    \ setfiletype json
