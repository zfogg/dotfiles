" ftplugin/python
scriptencoding utf-8


setl ts=4
    \ expandtab
    \ cinwords=if,elif,else,for,while,try,except,finally,def,class,with
    \ errorformat=%C\ %.%#,%A\ \ File\ \"%f\"\\,\ line\ %l%.%#,%Z%[%^\ ]%\\@=%m

"setl omnifunc=pythoncomplete#Complete
