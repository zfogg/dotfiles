-- lua/rc/treesitter.lua

-- nvim-treesitter v1.0+ only manages parser installation.
-- Highlighting is now handled by Neovim's built-in vim.treesitter module.
require'nvim-treesitter'.setup {
  ensure_installed = {},
  auto_install = false,
  ignore_install = {
    "ipkg",
  },
}
