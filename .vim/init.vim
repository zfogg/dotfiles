"  vim: filetype=vim:
scriptencoding utf-8

" VIMRC: ~/.vim/init.vim
" AUTHOR: Zachary Fogg <me@zfo.gg>
" DEPENDS:
"   NEOVIM: >= 0.4


try
    call z#rc#Init()
catch
    echom 'z#rc#Init() error :/'
    echom v:errmsg
finally
    runtime plugins.vim
    packloadall
endtry

silent! helptags ALL
