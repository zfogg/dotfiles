" after/filetype
scriptencoding utf-8


let g:c_syntax_for_h = 1


" NOTE: :help ft-syntax-omni
aug rc_after_filetype_omnicomplete
    au!
    au FileType php LanguageClientStart
aug END
