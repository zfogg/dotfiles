-- lua/zfogg/mappins.lua
local cmd = vim.cmd
local o_s = vim.o

vim.keymap.set({ "i", "x", "n", "s" }, "<D-s>", "<cmd>w<CR>", { desc = "Save file" })
vim.keymap.set({ "i", "x", "n", "s" }, "<C-s>", "<cmd>w<CR>", { desc = "Save file" })
--vim.keymap.set({ "i" }, ",W", "<Esc>:w<CR>a", { desc = "Save file" })

vim.cmd [[
  "command! -nargs=+ -complete=command Redir let s:reg = @@ | redir @"> | silent execute <q-args> | redir END | new | pu | 1,2d_ | let @@ = s:reg
  "":nnoremap <C-h> :TmuxNavigateLeft<CR>
]]
