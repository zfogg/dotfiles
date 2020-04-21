" plugin/rc/fzf
scriptencoding utf-8



command! -bang -nargs=? -complete=dir Files
    \ call fzf#vim#files(<q-args>,
        \ {'options': [
            \ '--layout=reverse',
            \ '--info=inline',
            \ '--preview',
            \ '~/.vim/bundle/fzf.vim/bin/preview.sh {}',
            \ ],
        \ },
    \ <bang>0)

if exists('$TMUX')
    command! -bang -nargs=* Find call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --follow --color "always" '.shellescape(<q-args>).'| tr -d "\017"', 1, <bang>0
else
    command! -bang -nargs=* Find call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --follow --color "always" '.shellescape(<q-args>), 1, <bang>0)
endif

