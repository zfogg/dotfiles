-- lua/rc/ale.lua

local M = {}

function M.setup()
  vim.cmd [[
  "let s:show=PHas('nvim-ale-diagnostic')
  let s:show=1

  let g:ale_enabled=1

  let g:ale_solidity_solc_executable='solc'
  let g:ale_solidity_solc_options='@openzeppelin=node_modules/@openzeppelin hardhat=node_modules/hardhat'

  if exists('g:vscode')
    let g:alet_set_loclist=1
    let g:alet_set_quickfix=0
  else
    let g:alet_set_loclist=1
    let g:alet_set_quickfix=0
  endif
  let g:ale_lsp_suggestions=1

  let g:ale_lint_on_text_changed=1
  let g:ale_lint_on_insert_leave=1
  let g:ale_lint_on_enter=1
  let g:ale_lint_on_save=1

  let g:ale_set_highlights=1
  let g:ale_set_signs=s:show
  let g:ale_echo_cursor=s:show
  let g:ale_hover_cursor=0
  " NOTE: Disabled to prevent duplicate virtual text with LSP diagnostics (clangd, etc.)
  " LSP handles virtual text via vim.diagnostic.config() in mason.lua
  let g:ale_virtualtext_cursor=0
  let g:ale_cursor_detail=0
  let g:ale_set_balloons=0

  let g:ale_floating_preview=0
  let g:ale_floating_window_preview=0
  let g:ale_hover_to_floating_preview=0
  let g:ale_detail_to_floating_preview=0

  let g:ale_hover_to_preview=0
  let g:ale_hover_to_floating_preview=0

  let g:ale_open_window=0
  let g:ale_open_list=0
  let g:ale_keep_list_window_open=0
  let g:ale_list_window_size=3

  let ts_linters = ['cspell', 'deno', 'tslint', 'tsserver', 'typecheck']

  let g:ale_linters={
  \ 'python': 'all',
  \ 'typescript': ts_linters, 'typescriptreact': ts_linters,
  \ 'lua': [],
  \ }

  " INFO: :h g:ale_open_list
  aug CloseLoclistWindowGroup
    au!
    au QuitPre * if empty(&buftype) | lclose | endif
  aug END
  ]]
end

function M.config()
end

return M
