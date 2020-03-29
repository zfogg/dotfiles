" plugin/rc/fzf
scriptencoding utf-8



"command! -bang -nargs=? -complete=dir Files
    "\ call fzf#vim#files(<q-args>,
        "\ {'options': [
            "\ '--layout=reverse',
            "\ '--info=inline',
            "\ '--preview',
            "\ '~/.vim/plugged/fzf.vim/bin/preview.sh {}',
            "\ ],
        "\ },
    "\ <bang>0)

