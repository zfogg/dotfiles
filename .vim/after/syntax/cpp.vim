" after/syntax/cpp
scriptencoding utf-8


if (&filetype !=? 'cpp')
    finish
endif


syn region foldPreprocessorIf
            \ start="^\#if"
            \ end="^\#endif"
            \ transparent
            \ fold
