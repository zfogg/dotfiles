-- rc/syn

-- INFO: https://stackoverflow.com/a/30552423/672346

local augroup = vim.api.nvim_create_augroup('vimrc_syn_todo', { clear = true })

vim.api.nvim_create_autocmd('Syntax', {
  group = augroup,
  callback = function()
    vim.cmd("syn match MyTodo /\\v<(FIXME|NOTE|INFO|TODO|BUG)/ containedin=.*Comment,vimCommentTitle")
  end
})
