" vim: filetype=vim:
scriptencoding utf-8


try
    call z#rc#Init()
catch
    echom 'z#rc#Init() error :/'
    echom v:errmsg
finally
    packloadall
    runtime plugins.vim
endtry

silent! helptags ALL
