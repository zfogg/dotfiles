" ftplugin/c
scriptencoding utf-8


if (&filetype !=? 'c')
    finish
endif


setl ts=4

"setl fdm=marker fmr={,}
setl fdm=syntax
