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


Plug 'chriskempson/base16-vim' |
    \ Plug 'AfterColors.vim'
Plug 'AnsiEsc.vim'
Plug 'haya14busa/incsearch.vim'


call plug#end()
