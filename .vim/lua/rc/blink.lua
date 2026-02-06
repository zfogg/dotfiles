-- lua/rc/blink.lua
local M = {}

function M.setup()
  require('blink.cmp').setup({
    keymap = {
      preset = 'default',
      -- Disable Tab/S-Tab so Neocodeium can handle them
      ['<Tab>'] = {},
      ['<S-Tab>'] = {},
      ['<C-space>'] = { 'show', 'show_documentation', 'hide_documentation' },
      ['<C-e>'] = { 'cancel' },
      ['<C-y>'] = { 'accept', 'fallback' },
      ['<C-n>'] = { 'select_next', 'fallback' },
      ['<C-p>'] = { 'select_prev', 'fallback' },
    },
    completion = {
      menu = {
        auto_show = true,
        draw = {
          treesitter = { 'lsp' },
        },
      },
      documentation = {
        auto_show = true,
        window = {
          max_height = 10,
        },
      },
    },
    sources = {
      default = { 'lsp', 'path', 'snippets', 'buffer' },
    },
    signature = {
      enabled = true,
    },
  })
end

return M
