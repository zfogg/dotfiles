" autoload/z/constants/globals
scriptencoding utf-8


func! z#constants#globals#ConfigPaths() abort
    " g:mvvar =~# escape('s:\i+(_(l|f|r))?', '+()|?')
        " g:myvar_l  (l)ocal path basename
        " g:myvar_f  (f)ully resolved links (expanded vars)
        " g:myvar_r  (r)elative to $HOME (un-expanded vars)

    " dir/ ~/.dotfiles/ 'g:dotfiles'
    let g:dotfiles_l = '.dotfiles'
    let g:dotfiles   = fnamemodify(exists('$DOTFILES') ? $DOTFILES : g:dotfiles_l, ':~')
    let g:dotfiles_f = expand(g:dotfiles)
    let g:dotfiles_r = fnamemodify(g:dotfiles, ':~')

    " dir/ ~/.vim/ 'g:dotvim'
    let g:dotvim_l   = '.vim'
    let g:dotvim     = exists('$DOTVIM')               ? $DOTVIM   : g:dotfiles.'/'.g:dotvim_l
    let g:dotvim_f   = expand(g:dotvim)
    let g:dotvim_r   = fnamemodify(g:dotvim,   ':~')

    " file ~/.vim/init.vim 'g:myvimrc'
    let g:myvimrc_l = 'init.vim'
    let g:myvimrc   = g:dotvim  .'/'.g:myvimrc_l
    let g:myvimrc_f = g:dotvim_f.'/'.g:myvimrc_l
    let g:myvimrc_r = g:dotvim_r.'/'.g:myvimrc_l
endfunc


func! z#constants#globals#Python() abort
    if exists('$PYENV_ROOT')
        let g:python_host_prog  = $PYENV_ROOT.'/versions/neovim2/bin/python'
        let g:python3_host_prog = $PYENV_ROOT.'/versions/neovim3/bin/python'
    elseif exists('$BREW')
        let g:python_host_prog  = $BREW.'/bin/python2.7'
        let g:python3_host_prog = $BREW.'/bin/python3.5'
    endif
endfunc


func! z#constants#globals#Vimpager() abort
    " V1
    let g:vimpager = {
        \ 'enabled'     : 0,
        \ 'X11'         : 0,
        \ 'ansiesc'     : 0,
        \ 'passthrough' : 1,
    \ }
    let g:less     = {
        \ 'enabled'     : 1,
        \ 'number'      : 1,
        \ 'hlsearch'    : 1,
        \ 'scrolloff'   : 5,
    \ }
    " V2
    let g:vimpager_use_gvim        = 0
    let g:vimpager_passthrough     = 1
    let g:vimpager_disable_x11     = 1
    let g:vimpager_scrolloff       = 5
    let g:vimpager_disable_ansiesc = 1
endfunc
