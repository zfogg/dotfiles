-- plugin/after/nightfox.lua

if not _G.PHas('nightfox.nvim') then return end

vim.cmd('colorscheme carbonfox')

local bgcolor = "#464646"

-- Fix cursor visibility on light backgrounds (e.g., git diff highlights)
vim.api.nvim_create_autocmd("ColorScheme", {
  pattern = "*",
  callback = function()
    -- Set cursor background to faded green for visibility
    vim.api.nvim_set_hl(0, 'Cursor', { bg = bgcolor })
    vim.api.nvim_set_hl(0, 'lCursor', { bg = bgcolor })
    vim.api.nvim_set_hl(0, 'TermCursor', { bg = bgcolor })
  end,
})

-- Apply immediately
vim.api.nvim_set_hl(0, 'Cursor', { bg = bgcolor })
vim.api.nvim_set_hl(0, 'lCursor', { bg = bgcolor })
vim.api.nvim_set_hl(0, 'TermCursor', { bg = bgcolor })
