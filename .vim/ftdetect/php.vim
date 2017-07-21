" ftdetect/php


au BufRead,BufNewFile
        \ *.php,*.phpt
    \ setfiletype php


let g:neomake_php_enabled_makers = get(g:, 'neomake_php_enabled_makers', [
        \ 'php',
        \ 'phpcs',
    \ ])
