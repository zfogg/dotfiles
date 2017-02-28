" racer
scriptencoding utf-8


let g:racer_cmd = exepath('racer')
let g:racer_experimental_completer = 1


func! <SID>RacerMaps() abort
    nmap <buffer> gd <Plug>(rust-def)
    nmap <buffer> gs <Plug>(rust-def-split)
    nmap <buffer> gx <Plug>(rust-def-vertical)
    nmap <buffer> <S-k> <Plug>(rust-doc)
endfunc

aug RcPlugin__racer
    au!
    au FileType rust call <SID>RacerMaps()
aug END
