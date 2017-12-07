" autoload/z/keys


func! z#keys#Tmux() abort
    if empty($TMUX)
        nmap <C-H> <C-W>h
        nmap <C-J> <C-W>j
        nmap <C-K> <C-W>k
        nmap <C-L> <C-W>l
    else
        let g:tmux_navigator_no_mappings = 1
        nmap <silent> <C-h> :TmuxNavigateLeft<CR>
        nmap <silent> <C-j> :TmuxNavigateDown<CR>
        nmap <silent> <C-k> :TmuxNavigateUp<CR>
        nmap <silent> <C-l> :TmuxNavigateRight<CR>
        nmap <silent> <C-\> :TmuxNavigatePrevious<CR>
    end
    if has('nvim')
        "let $NVIM_TUI_ENABLE_CURSOR_SHAPE = 1 " change cursor to bar in insert mode
        tmap          <Esc> <C-\><C-n>
        tmap <silent> jj    <C-\><C-n>{}k$
        tmap <silent> <C-j> jj<C-j>
        tmap <silent> <C-k> jj<C-k>
        tmap <silent> <C-l> jj<C-l>
    end
endfunc
