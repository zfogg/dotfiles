" rc/util
"  vim: foldmethod=marker:
scriptencoding utf-8


func! PHas(plugin) abort
  "return call(function('z#util#HasPlugin'), a:000)
  return !empty(luaeval('packer_plugins["'.a:plugin.'"]'))
endfunc
