" plugin/cursorline


aug PluginConfig_cursorline
    au!
    au BufReadPost * call cursorline#RestorePosition()
    au InsertLeave,WinEnter,TabEnter,BufEnter *
        \setl   cursorline   cursorcolumn
    au InsertEnter,WinLeave,TabLeave *
        \setl nocursorline nocursorcolumn
aug END
