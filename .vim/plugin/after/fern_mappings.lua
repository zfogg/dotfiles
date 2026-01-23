-- plugin/after/fern_mappings
-- Keymaps for Fern - these load immediately, but Fern itself is lazy-loaded

vim.keymap.set('n', '<Leader>n<Space>', ':Fern . -drawer -toggle<CR>', { silent = true })
vim.keymap.set('n', '<Leader>nn', ':Fern . -drawer -wait -reveal=%<BAR>wincmd p<CR>', { silent = true })
