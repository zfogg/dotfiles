-- lua/rc/ale.lua

vim.cmd [[
let g:ale_enabled=1

let g:ale_solidity_solc_executable='solc'
let g:ale_solidity_solc_options='@openzeppelin=node_modules/@openzeppelin'
let g:alet_set_loclist=1
let g:alet_set_quickfix=0
let g:ale_lsp_suggestions=1

let g:ale_lint_on_text_changed=1
let g:ale_lint_on_insert_leave=1
let g:ale_lint_on_enter=1
let g:ale_lint_on_save=1

let g:ale_set_highlights=1
let g:ale_set_signs=0
let g:ale_echo_cursor=1
let g:ale_virtualtext_cursor=0
let g:ale_cursor_detail=1
let g:ale_set_balloons=1

let g:ale_floating_preview=1
let g:ale_floating_window_preview=1
let g:ale_hover_to_floating_preview=1
let g:ale_detail_to_floating_preview=1

let g:ale_hover_to_preview=1
let g:ale_hover_to_floating_preview=1

let g:ale_open_window=1
let g:ale_open_list=1
let g:ale_keep_list_window_open=0
let g:ale_list_window_size=7

" INFO: :h g:ale_open_list
aug CloseLoclistWindowGroup
  au!
  au QuitPre * if empty(&buftype) | lclose | endif
aug END
]]
