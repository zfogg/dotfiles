-- init.lua

vim.fn['z#rc#Init']()

vim.cmd [[packadd packer.nvim]]
vim.cmd [[packadd vimball]]

require 'zfogg'
vim.cmd([[autocmd BufWritePost plugins.lua source <afile> | PackerCompile]])
