" autoload/z/indentguides


func! z#nerdtree#AutoCwd() abort
    if exists('b:noNERDTreeAutoCWD')
        return
    elseif exists('g:zNTOn')
        "cd %:p:h
        NERDTreeMirror
        wincmd p
    endif
endfunc


func! z#nerdtree#Toggle() abort
    if exists('t:NERDTreeBufName')
        if exists('g:zNTOn') || &buftype !=# ''
            unlet! g:zNTOn
            NERDTreeClose
            return
        else
            NERDTreeMirrorToggle
        endif
    else
        NERDTree %:p:h
    endif
    let g:zNTOn = 1
    wincmd p
endfunc
