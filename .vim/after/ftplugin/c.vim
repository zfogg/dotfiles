" ftplugin/c
scriptencoding utf-8


if (&filetype !=? 'c')
    finish
endif


setl sw=4 ts=4 sts=4

"setl fdm=marker fmr={,}
setl fdm=syntax
