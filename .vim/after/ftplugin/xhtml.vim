" ftplugin/xhtml
scriptencoding utf-8

if exists('b:did_load_filetypes_userafter')
  finish
endif
let b:did_load_filetypes_userafter = 1


so expand('<sfile>:p:h').'/html.vim'
