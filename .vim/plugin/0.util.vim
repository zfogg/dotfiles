" rc/util
"  vim: foldmethod=marker:
scriptencoding utf-8


func! PHas(plugin) abort
  if !has('nvim') | return 0 | endif
  " Use the global Lua PHas function if available
  if exists('*luaeval')
    return luaeval('_G.PHas and _G.PHas(_A) or 0', a:plugin)
  endif
  return 0
endfunc
