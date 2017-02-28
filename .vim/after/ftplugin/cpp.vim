" ftplugin/cpp
scriptencoding utf-8


if (&filetype !=? 'cpp')
    finish
endif


setl ts=4

"setl fdm=marker fmr={,}
setl makeprg=make
