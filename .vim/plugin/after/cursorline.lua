-- rc/cursorline

if vim.g.rc_cursorline == nil then
  vim.g.rc_cursorline = true
end

if vim.g.rc_cursorline then
  local augroup = vim.api.nvim_create_augroup('PluginConfig_cursorline', { clear = true })

  vim.api.nvim_create_autocmd('BufReadPost', {
    group = augroup,
    callback = function()
      -- Note: z#cursorline#RestorePosition() would need to be implemented in Lua
    end
  })

  vim.api.nvim_create_autocmd({ 'InsertLeave', 'WinEnter', 'TabEnter', 'BufEnter' }, {
    group = augroup,
    callback = function()
      vim.opt_local.cursorline = true
      vim.opt_local.cursorcolumn = true
    end
  })

  vim.api.nvim_create_autocmd({ 'InsertEnter', 'WinLeave', 'TabLeave' }, {
    group = augroup,
    callback = function()
      vim.opt_local.cursorline = false
      vim.opt_local.cursorcolumn = false
    end
  })
end
