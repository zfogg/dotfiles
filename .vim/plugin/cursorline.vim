" plugin/cursorline


aug PluginConfig_cursorline
    au!
    au BufReadPost * call cursorline#RestorePosition()
    au InsertLeave,WinEnter,TabEnter,BufEnter *
        \set   cursorline
    au InsertEnter,WinLeave,TabLeave *
        \set nocursorline
aug END
