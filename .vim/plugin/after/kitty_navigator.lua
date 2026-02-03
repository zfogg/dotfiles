-- plugin/after/kitty_navigator.lua
-- Tell Kitty when nvim is running (works over SSH too)

-- Only run if TERM indicates kitty
if not (vim.env.TERM or ''):match('kitty') then return end

-- Set user var on startup
vim.fn.system('kitten @ set-user-vars IS_NVIM=true')

-- Clear on exit
vim.api.nvim_create_autocmd('VimLeavePre', {
  callback = function()
    vim.fn.system('kitten @ set-user-vars IS_NVIM=')
  end,
})
