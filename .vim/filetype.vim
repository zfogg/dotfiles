" filetype


let g:c_syntax_for_h = 1


" NOTE: :help ft-syntax-omni
aug rc_filetype_omnicomplete
    au!
    au FileType python     setl ofu=pythoncomplete#Complete
    au FileType html       setl ofu=htmlcomplete#CompleteTags
    au FileType css        setl ofu=csscomplete#CompleteCSS
    au FileType xml        setl ofu=xmlcomplete#CompleteTags
    "au FileType php        setl ofu=phpcomplete#CompletePHP
    au FileType c          setl ofu=ccomplete#Complete
    au FileType sql        setl ofu=sqlcomplete#Complete

    "au FileType javascript setl ofu=javascriptcomplete#CompleteJS

    au FileType *
        \ if &ofu == '' |  setl ofu=syntaxcomplete#Complete | endif

    au FileType crontab setlocal nobackup nowritebackup
aug END


" INFO: https://github.com/chrisbra/csv.vim#manual-installation
"if exists("did_load_csvfiletype") | finish | endif
"let did_load_csvfiletype=1
"augroup filetypedetect
    "au!
    "au BufRead,BufNewFile *.csv,*.dat setfiletype csv
"augroup END
