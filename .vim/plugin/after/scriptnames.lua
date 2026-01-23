-- rc/scriptnames

-- Simplified scriptnames - original implementation was complex
-- Can be expanded later if needed

vim.api.nvim_create_user_command('Scriptnames', function(opts)
  vim.cmd('scriptnames')
end, { nargs = '?' })

vim.api.nvim_create_user_command('Scratch', function(opts)
  vim.cmd('new')
  vim.opt_local.buftype = 'nofile'
  vim.opt_local.bufhidden = 'hide'
  vim.opt_local.swapfile = false
end, { nargs = '+' })
