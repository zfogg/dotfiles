" autoload/z/php
scriptencoding utf-8


func! z#php#SyntaxOverride() abort
  hi! def link phpDocTags  phpDefine
  hi! def link phpDocParam phpType
endfunc
