-- init.lua

vim.g.do_filetype_lua    = 1
vim.g.did_load_filetypes = 0

vim.fn['z#rc#Init']()

require('zfogg')
