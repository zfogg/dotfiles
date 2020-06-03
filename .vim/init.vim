" vim: filetype=vim:
scriptencoding utf-8


try
    call z#rc#Init()
catch
    echom 'z#rc#Init() error :/'
    echom v:errmsg
finally
    filetype plugin on
    syntax on
    runtime plugins.vim
    packloadall
endtry

silent! helptags ALL
