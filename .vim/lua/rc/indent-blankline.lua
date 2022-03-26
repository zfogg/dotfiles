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
  require("indent_blankline")
    .setup({
      space_char_blankline = " ",
      show_current_context = true,
      show_current_context_start = true,
      show_trailing_blankline_indent = false,
      use_treesitter = true,
      char_highlight_list = {
        --"IndentBlanklineIndent1",
        "IndentBlanklineIndent2",
        "IndentBlanklineIndent3",
        "IndentBlanklineIndent4",
        "IndentBlanklineIndent5",
        "IndentBlanklineIndent6",
      },
      filetype_exclude = {
        'nerdtree',
        'packer',
        'fern',
        'lspinfo',
        'packer',
        'checkhealth',
        'help',
        '',
      },
      buftype_exclude = {
        'terminal',
      },
      char_list = {
        '▏',
        '▏',
        '▏',
        '▏',
        '▏',
        '▏',
      },
      context_char_list = {
        '┃',
        '┃',
        '┃',
        '┃',
        '┃',
        '┃',
      },
    })

  vim.cmd[[
    highlight IndentBlanklineChar ctermfg=NONE cterm=nocombine guifg=NONE gui=nocombine
    highlight IndentBlanklineContextStart guisp=#71b56c gui=underlineline,bold,nocombine cterm=underlineline,bold,nocombine
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
