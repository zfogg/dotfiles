-- rc/session

if not _G.PHas('vim-session') then return end

vim.g.session_default_name = 'default'
vim.g.session_command_aliases = 1
vim.g.session_autosave = 'yes'
vim.g.session_autosave_periodic = 2
vim.g.session_autoload = 0
vim.g.session_verbose_messages = 0
vim.g.session_persist_globals = { '&sessionoptions' }

table.insert(vim.g.session_persist_globals, 'g:session_autosave')
table.insert(vim.g.session_persist_globals, 'g:session_autosave_periodic')
table.insert(vim.g.session_persist_globals, 'g:session_autoload')
table.insert(vim.g.session_persist_globals, 'g:session_default_to_last')
table.insert(vim.g.session_persist_globals, 'g:session_persist_globals')

vim.keymap.set('n', '<Leader>So', ':OpenSession<CR>')
vim.keymap.set('n', '<Leader>Ss', ':SaveSession<CR>')
vim.keymap.set('n', '<Leader>Sq', ':SaveSession<CR>:CloseSession<CR>')
vim.keymap.set('n', '<Leader>SQ', '<Leader>Sq:q<CR>')
