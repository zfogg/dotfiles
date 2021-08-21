" vim: filetype=vim:
scriptencoding utf-8


try
    call z#rc#Init()
catch
    echom v:errmsg
    echom 'z#rc#Init() error :/'
finally
    packloadall
    runtime plugins.vim
endtry

silent! helptags ALL
