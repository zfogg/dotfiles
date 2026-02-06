-- plugin/after/nightfox.lua

if not _G.PHas('nightfox.nvim') then return end

vim.cmd('colorscheme carbonfox')

-- Fix cursor visibility on light backgrounds (e.g., git diff highlights)
vim.api.nvim_create_autocmd("ColorScheme", {
  pattern = "*",
  callback = function()
    -- Set cursor background to dark faded yellow for visibility
    vim.api.nvim_set_hl(0, 'Cursor', { bg = '#5a5a30' })
    vim.api.nvim_set_hl(0, 'lCursor', { bg = '#5a5a30' })
    vim.api.nvim_set_hl(0, 'TermCursor', { bg = '#5a5a30' })
  end,
})

-- Apply immediately
vim.api.nvim_set_hl(0, 'Cursor', { bg = '#5a5a30' })
vim.api.nvim_set_hl(0, 'lCursor', { bg = '#5a5a30' })
vim.api.nvim_set_hl(0, 'TermCursor', { bg = '#5a5a30' })
