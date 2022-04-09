-- lua/rc/symbols-outline.lua
--local map = require('zfogg.util').map

local M = {}

local function keymaps()
  local m = require('rc.mapx')

  m.nname('<Leader>m', 'symbols & tags')
  nnoremap('<Leader>m<Space>', ':SymbolsOutline<CR>',      'silent')
  nnoremap('<Leader>mm',       ':SymbolsOutlineOpen<CR>',  'silent')
  nnoremap('<Leader>mq',       ':SymbolsOutlineClose<CR>', 'silent')
end

-- INFO: https://github.com/simrat39/symbols-outline.nvim#configuration
function M.setup()
  vim.g.symbols_outline = {
    highlight_hovered_item = true,
    show_guides            = true,
    auto_preview           = true,
    position               = 'right',
    relative_width         = true,
    width                  = 12,
    auto_close             = false,
    show_numbers           = false,
    show_relative_numbers  = false,
    show_symbol_details    = true,
    preview_bg_highlight   = 'Pmenu',
    keymaps = { -- These keymaps can be a string or a table for multiple keys
      close          = {"<Esc>", "q"},
      goto_location  = "<Cr>",
      focus_location = "o",
      hover_symbol   = "<C-space>",
      toggle_preview = "K",
      rename_symbol  = "r",
      code_actions   = "a",
    },
    lsp_blacklist = {},
    symbol_blacklist = {},
    symbols = {
      File          = {icon = "",    hl = "TSURI"},
      Module        = {icon = "",    hl = "TSNamespace"},
      Namespace     = {icon = "",    hl = "TSNamespace"},
      Package       = {icon = "",    hl = "TSNamespace"},
      Class         = {icon = "𝓒",    hl = "TSType"},
      Method        = {icon = "ƒ",    hl = "TSMethod"},
      Property      = {icon = "",    hl = "TSMethod"},
      Field         = {icon = "",    hl = "TSField"},
      Constructor   = {icon = "",    hl = "TSConstructor"},
      Enum          = {icon = "ℰ",    hl = "TSType"},
      Interface     = {icon = "ﰮ",    hl = "TSType"},
      Function      = {icon = "",    hl = "TSFunction"},
      Variable      = {icon = "",    hl = "TSConstant"},
      Constant      = {icon = "",    hl = "TSConstant"},
      String        = {icon = "𝓐",    hl = "TSString"},
      Number        = {icon = "#",    hl = "TSNumber"},
      Boolean       = {icon = "⊨",    hl = "TSBoolean"},
      Array         = {icon = "",    hl = "TSConstant"},
      Object        = {icon = "⦿",    hl = "TSType"},
      Key           = {icon = "🔐",   hl = "TSType"},
      Null          = {icon = "NULL", hl = "TSType"},
      EnumMember    = {icon = "",    hl = "TSField"},
      Struct        = {icon = "𝓢",    hl = "TSType"},
      Event         = {icon = "🗲",    hl = "TSType"},
      Operator      = {icon = "+",    hl = "TSOperator"},
      TypeParameter = {icon = "𝙏",    hl = "TSParameter"},
    }
  }

  keymaps()
end

function M.config()
end

return M
