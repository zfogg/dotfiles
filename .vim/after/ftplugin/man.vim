" ftplugin/man
scriptencoding utf-8

if exists('b:did_load_filetypes_userafter')
  finish
endif
let b:did_load_filetypes_userafter = 1


setl nonu nornu


redraw!
echo getcwd()
