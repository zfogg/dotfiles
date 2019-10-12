" plugin/rc/cursorline
scriptencoding utf-8


aug PluginConfig_cursorline
    au!

    au BufReadPost * call z#cursorline#RestorePosition()

    au InsertLeave,WinEnter,TabEnter,BufEnter *
        \setl   cursorline   cursorcolumn

    au InsertEnter,WinLeave,TabLeave *
        \setl nocursorline nocursorcolumn
aug END

