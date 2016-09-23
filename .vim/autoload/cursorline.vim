" autoload/cursorline/restore


func! cursorline#RestorePosition() abort
    if !exists('g:leave_my_cursor_position_alone')
        let g:cursor_line = line("'\"")
        if g:cursor_line > 0 && g:cursor_line <= line('$')
            exe "normal g'\""
        endif
    endif
endfunc
