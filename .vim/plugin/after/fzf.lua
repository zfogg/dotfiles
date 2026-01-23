-- rc/fzf

if not _G.PHas('fzf.vim') then return end

vim.g.fzf_preview_window = { 'right:60%', 'ctrl-/' }
vim.g.fzf_buffers_jump = 1

if vim.env.FZF_HISTORY_DIR then
  vim.g.fzf_history_dir = vim.fn.expand('$FZF_HISTORY_DIR')
else
  vim.g.fzf_history_dir = vim.env.HOME .. '/.fzf-history'
end
vim.fn.mkdir(vim.g.fzf_history_dir, 'p')

vim.g.fzf_colors = {
  fg = { 'fg', 'Normal' },
  bg = { 'bg', 'Normal' },
  hl = { 'fg', 'Comment' },
  ['fg+'] = { 'fg', 'CursorLine', 'CursorColumn', 'Normal' },
  ['bg+'] = { 'bg', 'CursorLine', 'CursorColumn' },
  ['hl+'] = { 'fg', 'Statement' },
  info = { 'fg', 'PreProc' },
  border = { 'fg', 'Ignore' },
  prompt = { 'fg', 'Conditional' },
  pointer = { 'fg', 'Exception' },
  marker = { 'fg', 'Keyword' },
  spinner = { 'fg', 'Label' },
  header = { 'fg', 'Comment' }
}

vim.g.fzf_action = {
  ['ctrl-t'] = 'tab split',
  ['ctrl-s'] = 'split',
  ['ctrl-v'] = 'vsplit',
}

-- Hide status line when fzf is active
if vim.fn.has('nvim') == 1 and not vim.g.fzf_layout then
  local fzf_augroup = vim.api.nvim_create_augroup('fzf-custom', { clear = true })
  vim.api.nvim_create_autocmd('FileType', {
    group = fzf_augroup,
    pattern = 'fzf',
    callback = function()
      vim.opt_local.laststatus = 0
      vim.opt_local.showmode = false
      vim.opt_local.ruler = false

      vim.api.nvim_create_autocmd('BufLeave', {
        buffer = 0,
        callback = function()
          vim.opt.laststatus = 2
          vim.opt.showmode = true
          vim.opt.ruler = true
        end
      })
    end
  })
end

-- Setup Find command with rg if available
if vim.fn.executable('rg') == 1 then
  local find_cmd = 'rg'
    .. ' --fixed-strings'
    .. ' --ignore-case'
    .. ' --follow'
    .. ' --color "always"'

  if vim.env.TMUX then
    vim.api.nvim_create_user_command('Find', function(args)
      local query = vim.fn.shellescape(args.args)
      vim.fn['fzf#vim#grep'](find_cmd .. ' ' .. query .. '| tr -d "\027"', 1, 0)
    end, { nargs = '*' })
  else
    vim.api.nvim_create_user_command('Find', function(args)
      local query = vim.fn.shellescape(args.args)
      vim.fn['fzf#vim#grep'](find_cmd .. ' ' .. query, 1, 0)
    end, { nargs = '*' })
  end
end
