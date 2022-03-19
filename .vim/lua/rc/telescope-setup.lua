-- lua/rc/telescope-setup.lua
local map = require('zfogg.util').map

local silent = { silent = true }
-- Navigate buffers and repos
map('n', '<c-s-a>', [[<cmd>Telescope buffers show_all_buffers=true theme=get_dropdown<cr>]], silent)
map('n', '<c-e>',   [[<cmd>Telescope frecency   theme=get_dropdown<cr>]], silent)
map('n', '<c-g>',   [[<cmd>Telescope git_files  theme=get_dropdown<cr>]], silent)
map('n', '<c-p>',   [[<cmd>Telescope find_files theme=get_dropdown<cr>]], silent)
map('n', '<c-f>',   [[<cmd>Telescope live_grep  theme=get_dropdown<cr>]], silent)
