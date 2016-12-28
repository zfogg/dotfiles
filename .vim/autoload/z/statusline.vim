" autoload/z/rc
scriptencoding utf-8


func! z#statusline#HiClear() abort
    hi clear StatusLine
    hi clear StatusLineNC
    hi clear User1
endfunc


" solarized
func! z#statusline#Highlight() abort
    call z#statusline#HiClear()

    hi StatusLine
        \ term=reverse cterm=reverse gui=reverse
        \ ctermfg=14    ctermbg=8
        \ guifg=#93a1a1 guibg=#002b36

    hi StatusLineNC
        \ term=reverse cterm=reverse gui=reverse
        \ ctermfg=11    ctermbg=0
        \ guifg=#657b83 guibg=#073642

    hi User1
        \ ctermfg=14    ctermbg=0
        \ guifg=#93a1a1 guibg=#073642
endfunc
