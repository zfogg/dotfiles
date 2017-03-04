" autoload/z/rc


func! z#rc#Init() abort
    call z#constants#globals#Python()
    call z#constants#globals#ConfigPaths()
endfunc
