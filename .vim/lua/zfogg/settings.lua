-- lua/zfogg/settings.lua


-- INFO: :help g:do_filetype_lua
vim.g.do_filetype_lua    = 1
vim.g.did_load_filetypes = 0

local kitty_augroup      = vim.api.nvim_create_augroup("kitty_conf", { clear = true })
vim.api.nvim_create_autocmd("BufWritePost", {
  pattern  = "*/.config/kitty/*.conf",
  group    = kitty_augroup,
  callback = function()
    vim.cmd [[
      silent !kitty @ load-config
      redraw!
    ]]
  end,
})
