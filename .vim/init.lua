-- init.lua

vim.g.do_filetype_lua    = 1
vim.g.did_load_filetypes = 0

-- Define PHas early so plugin files can use it
function _G.PHas(plugin)
  local ok, lazy = pcall(require, "lazy.core.config")
  if ok and lazy.plugins and lazy.plugins[plugin] then
    return 1
  end
  return 0
end

vim.fn['z#rc#Init']()

require('zfogg')


