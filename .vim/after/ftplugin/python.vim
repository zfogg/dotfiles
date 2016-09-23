" ftplugin/python
scriptencoding utf-8

if exists('b:did_load_filetypes_userafter')
  finish
endif
let b:did_load_filetypes_userafter = 1


setl sw=4 sts=4
    \ expandtab
    \ cinwords=if,elif,else,for,while,try,except,finally,def,class,with
    \ errorformat=%C\ %.%#,%A\ \ File\ \"%f\"\\,\ line\ %l%.%#,%Z%[%^\ ]%\\@=%m

"setl omnifunc=pythoncomplete#Complete
