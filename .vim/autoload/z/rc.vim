" autoload/z/rc


func! z#rc#Init() abort
    call z#constants#globals#ConfigPaths()
    call z#constants#globals#Python()
endfunc
