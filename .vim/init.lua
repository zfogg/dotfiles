-- init.lua
-- vim: filetype=lua:

vim.fn['z#rc#Init']()

vim.cmd [[packadd packer.nvim]]
vim.cmd [[packadd vimball]]

require('plugins')
vim.cmd([[autocmd BufWritePost plugins.lua source <afile> | PackerCompile]])
