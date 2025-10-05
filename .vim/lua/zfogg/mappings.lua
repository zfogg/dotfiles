-- lua/zfogg/mappins.lua
local cmd = vim.cmd
local o_s = vim.o

vim.keymap.set({ "i", "x", "n", "s" }, "<D-s>", "<cmd>w<CR>", { desc = "Save file" })
vim.keymap.set({ "i", "x", "n", "s" }, "<C-s>", "<cmd>w<CR>", { desc = "Save file" })
--vim.keymap.set({ "i" }, ",W", "<Esc>:w<CR>a", { desc = "Save file" })

-- Window navigation with Ctrl-hjkl (fallback when tmux/kitty navigator disabled)
vim.keymap.set('n', '<C-h>', '<C-w>h', { desc = 'Move to left window', silent = true })
vim.keymap.set('n', '<C-j>', '<C-w>j', { desc = 'Move to window below', silent = true })
vim.keymap.set('n', '<C-k>', '<C-w>k', { desc = 'Move to window above', silent = true })
vim.keymap.set('n', '<C-l>', '<C-w>l', { desc = 'Move to right window', silent = true })

vim.cmd [[
  "command! -nargs=+ -complete=command Redir let s:reg = @@ | redir @"> | silent execute <q-args> | redir END | new | pu | 1,2d_ | let @@ = s:reg
  "":nnoremap <C-h> :TmuxNavigateLeft<CR>
]]
