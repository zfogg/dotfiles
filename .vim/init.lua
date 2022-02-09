-- init.lua

vim.fn['z#rc#Init']()

vim.cmd [[
packadd packer.nvim
packadd vimball
]]

require 'zfogg'

vim.cmd [[
silent! helptags ALL
]]
