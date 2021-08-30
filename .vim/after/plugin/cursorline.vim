" plugin/rc/cursorline
scriptencoding utf-8


aug PluginConfig_cursorline
    au!

    if get(g:, "rc_cursorline", v:true)
        au BufReadPost * call z#cursorline#RestorePosition()
    endif

    if get(g:, "rc_cursorline", v:true)
        au InsertLeave,WinEnter,TabEnter,BufEnter *
            \setl   cursorline   cursorcolumn

        au InsertEnter,WinLeave,TabLeave *
            \setl nocursorline nocursorcolumn
    endif
aug END

