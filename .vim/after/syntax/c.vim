" after/syntax/c
scriptencoding utf-8


if (&filetype !=? 'c')
    finish
endif


syn region foldPreprocessorIf
            \ start="^\#if"
            \ end="^\#endif"
            \ transparent
            \ fold
