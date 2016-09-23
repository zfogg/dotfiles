" plugin/cursorline


aug PluginConfig_cursorline
    au!
    au BufReadPost * call cursorline#RestorePosition()
aug END
