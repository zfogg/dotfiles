-- lua/rc/nightfox.lua

local M = {}

function M.setup()
end

function M.config()
  require('nightfox').setup({
    options = {
      -- Compiled file's destination location
      compile_path        = vim.fn.stdpath("cache") .. "/nightfox",
      compile_file_suffix = "_compiled", -- Compiled file suffix
      transparent         = false,    -- Disable setting background
      terminal_colors     = true, -- Set terminal colors (vim.g.terminal_color_*)
      dim_inactive        = true,   -- Non focused panes set to alternative background
      styles = {              -- Style to be applied to different syntax groups
        --bold underline underlineline
        --undercurl underdot underdash
        --reverse inverse italic standout
        --nocombine strikethrough
        comments  = "italic",
        functions = "italic,undercurl",
        keywords  = "bold",
        numbers   = "bold,italic,standout,nocombine",
        strings   = "italic",
        types     = "bold,underline",
        variables = "italic",
      },
      inverse = {             -- Inverse highlight for different types
        match_paren = false,
        visual      = false,
        search      = false,
      },
      modules = {
        barbar         = false,
        cmp            = false,
        dashboard      = false,
        fern           = true,
        fidget         = false,
        gitgutter      = false,
        gitsigns       = true,
        glyph_pallet   = true,
        hop            = false,
        illuminate     = false,
        lightspeed     = false,
        lsp_saga       = false,
        lsp_trouble    = false,
        modes          = false,
        native_lsp     = true,
        neogit         = false,
        neotree        = false,
        nvimtree       = false,
        pounce         = false,
        sneak          = false,
        symbol_outline = true,
        telescope      = true,
        treesitter     = true,
        tsrainbow      = false,
        whichkey       = false,
        diagnostic   = {
          enable     = true,
          background = true,
        },
      },
    },
  })
  -- Colorscheme is set in plugin/after/nightfox.lua
end

return M
