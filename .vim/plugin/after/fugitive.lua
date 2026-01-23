-- rc/fugitive

if not _G.PHas('vim-fugitive') then return end

vim.g.fugitive_pty = 0

vim.keymap.set('n', '<Leader>gc', ':Gcommit<CR>')
vim.keymap.set('n', '<Leader>gd', ':Gdiff<CR>')
vim.keymap.set('n', '<Leader>gl', ':Glog<CR>')
vim.keymap.set('n', '<Leader>gs', ':Gstatus<CR>')
vim.keymap.set('n', '<Leader>gR', ':Gread<CR>')
vim.keymap.set('n', '<Leader>gW', ':Gwrite<CR>:e<CR>')
vim.keymap.set('n', '<Leader>gp', ':Git push<CR>')
vim.keymap.set('n', '<Leader>gb', ':Gblame<CR>')
vim.keymap.set('n', '<Leader>gH', ':Gbrowse<CR>')
