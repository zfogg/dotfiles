" ftdetect/json


au BufRead,BufNewFile
        \ .jslintrc,.eslintrc,.jshintrc
        \,.babelrc
        \,.bowererrc
        \,composer.lock
        \,Pipfile.lock
    \ setfiletype json
