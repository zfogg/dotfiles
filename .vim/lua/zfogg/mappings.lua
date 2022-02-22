-- lua/zfogg/mappins.lua
local cmd = vim.cmd
local o_s = vim.o
local map_key = vim.api.nvim_set_keymap

vim.cmd [[
"command! -nargs=+ -complete=command Redir let s:reg = @@ | redir @"> | silent execute <q-args> | redir END | new | pu | 1,2d_ | let @@ = s:reg
"":nnoremap <C-h> :TmuxNavigateLeft<CR>
]]
