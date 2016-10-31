" rc/plugins_vimpager
scriptencoding utf-8


func! PIf(cond, ...) abort
    let l:opts = get(a:000, 0, {})
    let l:cond = get(a:, 'cond', v:false)
    return l:cond ? l:opts : extend(l:opts, { 'on': [], 'for': [] })
endfunc


call plug#begin('~/.vim/bundle')


Plug 'sheerun/vim-polyglot'


Plug 'tmux-plugins/vim-tmux-focus-events'    , PIf(!empty($TMUX)) |
    \ Plug 'christoomey/vim-tmux-navigator'  , PIf(!empty($TMUX))

Plug 'scrooloose/nerdtree'              |
    \Plug 'Xuyuanp/nerdtree-git-plugin' |
    \Plug 'taiansu/nerdtree-ag'         |
    \Plug 'jistr/vim-nerdtree-tabs'


Plug 'chriskempson/base16-vim' |
    \ Plug 'AfterColors.vim'
Plug 'itchyny/lightline.vim'
"Plug 'AnsiEsc.vim'
Plug 'ryanoasis/vim-devicons'
Plug 'machakann/vim-highlightedyank', { 'on': '<Plug>(highlightedyank)' }
Plug 'haya14busa/incsearch.vim'


call plug#end()
