" autoload/numberline


func! numberline#On() abort
    if !exists('b:nonumberline')
        setl number
    endif
endfunc

