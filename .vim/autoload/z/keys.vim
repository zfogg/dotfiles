" autoload/z/keys


func! z#keys#Tmux() abort
    if empty($TMUX)
        nmap <C-h> <C-w>h
        nmap <C-j> <C-w>j
        nmap <C-k> <C-w>k
        nmap <C-l> <C-w>l
    else
        let g:tmux_navigator_no_mappings = 1
        let g:tmux_navigator_disable_when_zoomed = 1
        nnoremap <silent> <C-h> :TmuxNavigateLeft<CR>
        nnoremap <silent> <C-j> :TmuxNavigateDown<CR>
        nnoremap <silent> <C-k> :TmuxNavigateUp<CR>
        nnoremap <silent> <C-l> :TmuxNavigateRight<CR>
        nnoremap <silent> <C-\> :TmuxNavigatePrevious<CR>
    end

    if has('nvim')
        if exists('g:vscode')
        else
            "let $NVIM_TUI_ENABLE_CURSOR_SHAPE = 1 " change cursor to bar in insert mode
            tmap          <Esc> <C-\><C-n>
            tmap <silent> jj    <C-\><C-n>{}k$
            tmap <silent> <C-j> jj<C-j>
            tmap <silent> <C-k> jj<C-k>
            tmap <silent> <C-l> jj<C-l>
        endif
    end
endfunc
