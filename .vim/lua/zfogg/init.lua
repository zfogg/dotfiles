-- lua/zfogg/init.lua

require 'zfogg.util'
require 'zfogg.settings'
require 'zfogg.plugins'
require 'zfogg.mappings'
require 'zfogg.colors'

vim.cmd [[
"au VimEnter * COQnow --shut-up
]]
