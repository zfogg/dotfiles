"  vim: filetype=vim:
scriptencoding utf-8

" VIMRC: ~/.vim/init.vim
" AUTHOR: Zach Fogg <me@zfo.gg>
" DEPENDS:
"   VIM:      >= 8.0
"   VIMPAGER: >= 2.06
"   NEOVIM:   >= 0.1.6


" {{{ config paths
    " x:mvvar =~# escape('s:\i+(_(l|f|r))?', '+()|?')
        " x:myvar_l  (l)ocal path basename
        " x:myvar_f  (f)ully resolved links (expanded vars)
        " x:myvar_r  (r)elative to $HOME (un-expanded vars)
        "
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
" }}} config paths


" {{{ rcfiles
    let s:rcfiles = {}
    for s:rcfile in [
        \ 'settings',
        \ 'keys',
        \ 'plugins',
    \ ]
        let s:rcfile_path = globpath(g:dotvim_f, 'rc/'.s:rcfile.'.vim', 0, 1)
        let s:rcfiles[s:rcfile] = get(s:rcfile_path, 0, '/dev/null')
    endfor
" }}} rcfiles


exe 'so '.s:rcfiles.settings

exe 'so '.s:rcfiles.keys

exe 'so '.s:rcfiles.plugins


" colorsc {{{1
    if has('termguicolors')
        set termguicolors
        let $NVIM_TUI_ENABLE_TRUE_COLOR=1
    endif
    try
        let g:base16colorspace  = 256
        let g:base16_shell_path = $BASE16_SHELL.'/scripts'
        if filereadable(expand('~/.vimrc_background'))
            source ~/.vimrc_background
        endif
    catch
        try
            colorscheme jellybeans
        catch /^Vim(colorscheme):E185/
            colorscheme desert
        endtry
    endt
" colorsc }}}1
