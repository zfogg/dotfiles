-- lua/zfogg/colors.lua
--
-- Terminal and display settings only.
-- Colorscheme is set by rc/nightfox.lua via lazy.nvim config.

vim.g.vimsyn_embed   = '1Pr'
vim.g.vimsyn_folding = 'aflPr'
vim.g.vimsyn_noerror = 1

vim.o.termguicolors = true
vim.o.termbidi      = true
vim.o.guicursor     = ''..
  'n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50'..
  ',a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor'..
  ',sm:block-blinkwait175-blinkoff150-blinkon175'

vim.g.base16colorspace = 256
