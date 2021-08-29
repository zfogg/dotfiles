" rc/util
"  vim: foldmethod=marker:
scriptencoding utf-8


func! PHas(...) abort
  "return call(function('z#util#HasPlugin'), a:000)
  return v:null != luaeval('packer_plugins["'.a:0.'"]')
endfunc
