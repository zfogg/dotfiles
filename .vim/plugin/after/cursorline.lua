-- rc/cursorline

if vim.g.rc_cursorline == nil then
  vim.g.rc_cursorline = true
end

if vim.g.rc_cursorline then
  local augroup = vim.api.nvim_create_augroup('PluginConfig_cursorline', { clear = true })

  vim.api.nvim_create_autocmd('BufReadPost', {
    group = augroup,
    callback = function()
      -- Restore cursor position to last known location
      if not vim.g.leave_my_cursor_position_alone then
        local mark = vim.api.nvim_buf_get_mark(0, '"')
        local line_count = vim.api.nvim_buf_line_count(0)
        if mark[1] > 0 and mark[1] <= line_count then
          vim.api.nvim_win_set_cursor(0, mark)
        end
      end
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
