" plugin/rc/localvimrc
scriptencoding utf-8


if z#util#HasPlugin('vim-localvimrc')
    let g:localvimrc_persistent = 2
    let g:localvimrc_sandbox=0
endif
