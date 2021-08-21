" plugin/rc/nvim-autopairs
scriptencoding utf-8


if !has('nvim') | finish | endif
if !PHas('nvim-autopairs') | finish | endif
if !PHas('nvim-compe') | finish | endif


lua << EOF
require('nvim-autopairs').setup{}

if 1 == vim.fn.PHas('compe-zsh') do
  require("nvim-autopairs.completion.compe").setup({
    map_cr = true, --  map <CR> on insert mode
    map_complete = true, -- it will auto insert `(` after select function or method item
    auto_select = false,  -- auto select first item
  })
endif
EOF
