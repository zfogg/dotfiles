-- lua/rc/indent-blanklint.lua

local M = {}

function M.setup()
  -- INFO: https://github.com/lukas-reineke/indent-blankline.nvim
  vim.opt.list = true
  --vim.opt.listchars:append("space:⋅")
  vim.opt.listchars:append("eol:↴")
  vim.opt.termguicolors = true
end

function M.config()
  --local util = require 'packer.util'
  require("ibl")
    .setup({
      exclude = {
        filetypes = {
          'nerdtree',
          'packer',
          'fern',
          'lspinfo',
          'packer',
          'checkhealth',
          'help',
          '',
        },
        buftypes = {
          'terminal',
        },
      },
    })

  vim.cmd[[
    highlight IndentBlanklineChar ctermfg=NONE cterm=nocombine guifg=NONE gui=nocombine
    highlight IndentBlanklineContextStart guisp=#71b56c gui=underline,bold,nocombine cterm=underline,bold,nocombine
    highlight IndentBlanklineContextChar ctermfg=11 cterm=nocombine guifg=#71b56c gui=nocombine
    highlight IndentBlanklineIndent1 guifg=#86c1b9 gui=nocombine
    highlight IndentBlanklineIndent2 guifg=#f7ca88 gui=nocombine
    highlight IndentBlanklineIndent3 guifg=#ca6b9f gui=nocombine
    highlight IndentBlanklineIndent4 guifg=#a1b56c gui=nocombine
    highlight IndentBlanklineIndent5 guifg=#7cafc2 gui=nocombine
    highlight IndentBlanklineIndent6 guifg=#a1b56c gui=nocombine
  ]]
end

return M
