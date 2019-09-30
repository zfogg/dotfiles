" autoload/z/indentguides
scriptencoding utf-8


func! z#indentguides#Highlight() abort
    hi clear IndentGuidesOdd
    hi clear IndentGuidesEven
    hi IndentGuidesOdd  ctermbg=19 guibg=#2f2f2f
    hi IndentGuidesEven ctermbg=08 guibg=#383838
endfunc

