" ftdetect/json


au BufRead,BufNewFile
        \ .jslintrc,.eslintrc,.jshintrc
        \,.babelrc
        \,.bowererrc
    \ setfiletype json
