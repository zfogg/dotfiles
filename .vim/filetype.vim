" filetype


let g:c_syntax_for_h = 1


" NOTE: :help ft-syntax-omni
" vint: -ProhibitAutocmdWithNoGroup
au FileType python     setl ofu=pythoncomplete#Complete
au FileType javascript setl ofu=javascriptcomplete#CompleteJS
au FileType html       setl ofu=htmlcomplete#CompleteTags
au FileType css        setl ofu=csscomplete#CompleteCSS
au FileType xml        setl ofu=xmlcomplete#CompleteTags
au FileType php        setl ofu=phpcomplete#CompletePHP
au FileType c          setl ofu=ccomplete#Complete
au Filetype *
    \ if &ofu == '' |  setl ofu=syntaxcomplete#Complete | endif
" vint: +ProhibitAutocmdWithNoGroup
