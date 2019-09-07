" autoload/z/nerdtree
scriptencoding utf-8


func! z#nerdtree#AutoCwd() abort
    if !&modifiable && exists('b:noNERDTreeAutoCWD')
        return
    else
        if z#nerdtree#isNERDTreeOpen()
            NERDTree %:p:h
            wincmd p
            NERDTreeTabsFind
        else
            NERDTreeCWD
            wincmd p
            NERDTreeTabsFind
        endif
    endif
endfunc


func! z#nerdtree#isNERDTreeOpen() abort
    return exists('t:NERDTreeBufName') && (bufwinnr(t:NERDTreeBufName) != -1)
endfunc
