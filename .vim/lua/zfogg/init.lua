-- lua/zfogg/init.lua
--
-- Load order matters:
--   1. util     - global helpers used by other modules
--   2. settings - vim options (no plugin deps)
--   3. mappings - keymaps + mapleader (MUST be before plugins so plugin/after/*.lua
--                 files can use <Leader> - lazy.nvim sources them during setup)
--   4. plugins  - lazy.nvim setup (sources plugin/after/*.lua during require("lazy").setup())
--                 Plugin-specific config that needs the plugin loaded goes in plugin/after/
--   5. colors   - terminal settings (termguicolors, guicursor)
--                 Colorscheme is set in plugin/after/nightfox.lua

require 'zfogg.util'
require 'zfogg.settings'
require 'zfogg.mappings'
require 'zfogg.plugins'
require 'zfogg.colors'

vim.cmd [[
"au VimEnter * COQnow --shut-up
]]
