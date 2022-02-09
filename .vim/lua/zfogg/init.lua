-- lua/zfogg/init.lua

require 'zfogg.util'
require 'zfogg.plugins'

vim.cmd [[
au VimEnter * COQnow --shut-up
]]
