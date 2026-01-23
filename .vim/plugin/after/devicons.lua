-- rc/devicons

vim.g.webdevicons_enable = 1
vim.g.webdevicons_enable_nerdtree = 1
vim.g.webdevicons_enable_unite = 0
vim.g.webdevicons_enable_vimfiler = 0
vim.g.webdevicons_enable_airline_tabline = 0
vim.g.webdevicons_enable_airline_statusline = 0
vim.g.webdevicons_enable_ctrlp = 0
vim.g.webdevicons_enable_startify = 0
vim.g.webdevicons_enable_flagship_statusline = 0

vim.g.WebDevIconsUnicodeDecorateFileNodes = 1
vim.g.webdevicons_conceal_nerdtree_brackets = 1
vim.g.WebDevIconsNerdTreeGitPluginForceVAlign = 1
vim.g.webdevicons_enable_denite = 1

if vim.fn.expand('$OSX') == '1' then
  vim.g.WebDevIconsOS = 'Darwin'
end
