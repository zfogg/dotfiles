-- rc/undotree

if not _G.PHas('undotree') then return end

vim.g.undotree_HighlightChangedWithSign = 1
vim.g.undotree_WindowLayout = 3
vim.g.undotree_SetFocusWhenToggle = 0
vim.g.undotree_DiffAutoOpen = 1

vim.keymap.set('n', '<Leader>u', ':UndotreeToggle<CR>')
