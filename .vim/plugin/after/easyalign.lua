-- rc/easyalign

vim.g.easy_align_ignore_groups = { 'Comment', 'String' }
vim.g.easy_align_interactive_modes = { 'l', 'r', 'c' }
vim.g.easy_align_bang_interactive_modes = { 'r', 'l', 'c' }

-- NOTE: requires tpope/vim-repeat
vim.keymap.set('n', 'ga', '<Plug>(EasyAlignRepeat)')
vim.keymap.set('x', 'ga', '<Plug>(EasyAlignRepeat)')
vim.keymap.set('n', 'gla', '<Plug>(LiveEasyAlignRepeat)')
vim.keymap.set('x', 'gla', '<Plug>(LiveEasyAlignRepeat)')
