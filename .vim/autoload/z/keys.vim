" autoload/z/keys


func! z#keys#Tmux() abort
    if empty($TMUX)
        nnoremap <C-H> <C-W>h
        nnoremap <C-J> <C-W>j
        nnoremap <C-K> <C-W>k
        nnoremap <C-L> <C-W>l
    else
        let g:tmux_navigator_no_mappings = 1
        nnoremap <silent> <C-h> :TmuxNavigateLeft<CR>
        nnoremap <silent> <C-j> :TmuxNavigateDown<CR>
        nnoremap <silent> <C-k> :TmuxNavigateUp<CR>
        nnoremap <silent> <C-l> :TmuxNavigateRight<CR>
        nnoremap <silent> <C-\> :TmuxNavigatePrevious<CR>
    end
    if has('nvim')
        let $NVIM_TUI_ENABLE_CURSOR_SHAPE = 1 " change cursor to bar in insert mode
        tnoremap <Esc> <C-\><C-n>
        tnoremap     <silent> jj <C-\><C-n>{}k$
        tnoremap     <silent> <C-j> jj<C-j>
        tnoremap     <silent> <C-k> jj<C-k>
        tnoremap     <silent> <C-l> jj<C-l>
    end
endfunc
