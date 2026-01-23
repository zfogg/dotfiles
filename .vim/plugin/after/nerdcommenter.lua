-- rc/nerdcommenter

vim.g.NERDSpaceDelims = 0
vim.g.NERDRemoveExtraSpaces = 1
vim.g.NERDCompactSexyComs = 0
vim.g.NERDDefaultAlign = 'both'
vim.g.NERDTrimTrailingWhitespace = 1
vim.g.NERDAltDelims_haskell = 1

local function Yo()
  vim.notify('yo', vim.log.levels.INFO)
end
vim.api.nvim_create_user_command('Yo', Yo, {})
