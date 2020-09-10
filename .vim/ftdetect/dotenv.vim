" ftdetect/dotenv


au BufRead,BufNewFile
        \  .env
        \ ,example.env,example.*.env
        \ ,.env.local
        \ ,.env.development.local
        \ ,.env.staging.local
        \ ,.env.production.local
        \ ,.env.test.local
        \ ,.envrc,example.envrc
    \ setfiletype sh
