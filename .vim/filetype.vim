" filetype


let g:c_syntax_for_h = 1


" NOTE: :help ft-syntax-omni
aug rc_filetype_omnicomplete
    au!
    au FileType python     setl ofu=pythoncomplete#Complete
    au FileType html       setl ofu=htmlcomplete#CompleteTags
    au FileType css        setl ofu=csscomplete#CompleteCSS
    au FileType xml        setl ofu=xmlcomplete#CompleteTags
    au FileType php        setl ofu=phpcomplete#CompletePHP
    au FileType c          setl ofu=ccomplete#Complete
    au FileType sql        setl ofu=sqlcomplete#Complete

    au FileType javascript setl ofu=javascriptcomplete#CompleteJS

    au Filetype *
        \ if &ofu == '' |  setl ofu=syntaxcomplete#Complete | endif
aug END
