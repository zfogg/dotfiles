" ftdetect/dotenv
scriptencoding utf-8


au BufRead,BufNewFile
        \ .env,env
        \,.example.env,.example.*.env,example.env,example.*.env
        \,.env.local,.env.*.local,env.local,env.*.local
        \,.envrc,.*.envrc,envrc
    \ setfiletype sh
