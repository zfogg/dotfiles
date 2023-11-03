" ftdetect/dotenv
scriptencoding utf-8


au BufRead,BufNewFile
        \ .env,env
        \,.example.env,.example.*.env,example.env,example.*.env
        \,.env.local,.env.*.local,env.local,env.*.local
        \,.envrc,.*.envrc,envrc
    \ setfiletype sh

au BufRead,BufNewFile .env.local set ft=sh
