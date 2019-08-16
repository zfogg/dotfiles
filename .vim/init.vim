"  vim: filetype=vim:
scriptencoding utf-8

" VIMRC: ~/.vim/init.vim
" AUTHOR: Zach Fogg <me@zfo.gg>
" DEPENDS:
"   VIM:      >= 8.0
"   VIMPAGER: >= 2.06
"   NEOVIM:   >= 0.1.6


call z#rc#Init()

let s:rcfiles = z#constants#rc#RcFiles()

exe 'so '.s:rcfiles.plugins
"exe 'so '.s:rcfiles.settings
"exe 'so '.s:rcfiles.statusline
"exe 'so '.s:rcfiles.keys
"exe 'so '.s:rcfiles.colorsc
