" autoload/z/nerdtree
scriptencoding utf-8


func! z#nerdtree#AutoCwd() abort
    if exists('b:noNERDTreeAutoCWD')
        return
    elseif exists('g:zNTOn')
        "cd %:p:h
        NERDTreeMirror
        "wincmd p
    endif
endfunc


func! z#nerdtree#isNERDTreeOpen() abort
    return exists('t:NERDTreeBufName') && (bufwinnr(t:NERDTreeBufName) != -1)
endfunc
