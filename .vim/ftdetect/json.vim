" ftdetect/json


au BufRead,BufNewFile
        \ .jslintrc,.eslintrc,.jshintrc
        \,.babelrc
        \,.bowererrc
        \,Pipfile.lock
    \ setfiletype json
