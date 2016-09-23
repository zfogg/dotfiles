" autoload/z/indentguides
scriptencoding utf-8


func! z#indentguides#Highlight() abort
    hi clear IndentGuidesOdd
    hi clear IndentGuidesEven
    hi IndentGuidesOdd  ctermbg=19 guibg=#383838
    hi IndentGuidesEven ctermbg=08 guibg=#585858
endfunc

