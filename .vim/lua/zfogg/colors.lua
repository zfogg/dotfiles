-- lua/zfogg/colors.lua


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

--vim.g.base16_shell_path = vim.fn.expand('~/.config/base16-shell')..'/scripts'

--vim.cmd('colorscheme base16-default-dark')
--vim.cmd('colorscheme nightfox')
