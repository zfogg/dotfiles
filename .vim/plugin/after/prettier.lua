-- rc/prettier

if not _G.PHas('vim-prettier') then return end

vim.g.prettier_autoformat_config_present = 1
vim.g.prettier_exec_cmd_async = 1
vim.g.prettier_quickfix_auto_focus = 0
