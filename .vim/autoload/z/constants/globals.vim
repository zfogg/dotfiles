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
    func! s:python_v(env, pythonn, short) abort
        let l:output=system('cd ~;'
            \.'p="`'.a:env.' '.a:pythonn.' -V 2>&1`";'
            \.(get(a:, 'short', v:false) == v:true?
                \'gsed -E "s!^Python\ ([23]\.[0-9]+)\..+!\1!"':
                \'gsed -E "s!^Python\ ([23]\.[0-9]+\.[0-9]+)!\1!"')
                    \.' <<< "$p"'
                    \." | xargs -0 printf '%s';")
        return substitute(l:output, '\n$', '', '')
    endfunc

    if exists('$PYENV_ROOT')
        let l:python_root = $PYENV_ROOT.'/versions/neovim'
        if     has('python3') | let l:python3_bin = 'bin/python3'
        elseif has('python2') | let l:python2_bin = 'bin/python2'
        endif
    elseif exists('$BREW')
        let l:python_root = $BREW
        if     has('python3') | let l:python3_bin = 'bin/python3'
        elseif has('python2') | let l:python2_bin = 'bin/python2'
        endif
    endif
    if     has('python3') | let g:python_host_prog = l:python_root.'/'.l:python3_bin
    elseif has('python2') | let g:python_host_prog = l:python_root.'/'.l:python2_bin
    endif
endfunc


func! z#constants#globals#Vimpager() abort
    " V1
    let g:vimpager = {
        \ 'enabled'     : 0,
        \ 'X11'         : 0,
        \ 'ansiesc'     : 1,
        \ 'passthrough' : 1,
    \ }
    let g:less     = {
        \ 'enabled'     : 1,
        \ 'number'      : 1,
        \ 'hlsearch'    : 1,
        \ 'scrolloff'   : 5,
    \ }
    " V2
    let vimpager_use_gvim        = 0
    let vimpager_passthrough     = 1
    let vimpager_disable_x11     = 1
    let vimpager_scrolloff       = 5
    let vimpager_disable_ansiesc = 1
endfunc
