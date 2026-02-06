-- lua/rc/autopairs.lua
local M = {}

local function keymaps()
  -- these mappings are coq recommended mappings unrelated to nvim-autopairs
  vim.keymap.set('i', '<Esc>',   [[pumvisible() ? "<C-e><Esc>" : "<Esc>"]], { expr = true })
  vim.keymap.set('i', '<C-c>',   [[pumvisible() ? "<C-e><C-c>" : "<C-c>"]], { expr = true })
  -- Don't override Tab if neocodeium is loaded - it handles Tab itself
  -- Only set Tab mapping if neocodeium is not available
  if not pcall(require, 'neocodeium') then
    vim.keymap.set('i', '<Tab>',   [[pumvisible() ? "<C-n>"      : "<Tab>"]], { expr = true })
  end
  vim.keymap.set('i', '<S-Tab>', [[pumvisible() ? "<C-p>"      : "<BS>"]],  { expr = true })
end

function M.setup()
  --local npairs = require('nvim-autopairs')
end

function M.config()
  local npairs = require('nvim-autopairs')

  npairs.setup({
      check_ts = true,
      map_bs   = false,
      map_cr   = false,
    })
  npairs.add_rules(require "nvim-autopairs.rules.endwise-lua")

  if PHas('coq_nvim') == 1 then
    local remap = vim.api.nvim_set_keymap
    vim.g.coq_settings = vim.g.coq_settings or {}
    vim.g.coq_settings = vim.tbl_deep_extend('force', vim.g.coq_settings, {
      keymap = { recommended = false, },
    })

    -- skip it, if you use another global object
    _G.MUtils= {}

    MUtils.CR = function()
      if vim.fn.pumvisible() ~= 0 then
        if vim.fn.complete_info({ 'selected' }).selected ~= -1 then
          return npairs.esc('<C-y>')
        else
          -- you can change <C-g><C-g> to <c-e> if you don't use other i_CTRL-X modes
          return npairs.esc('<C-g><C-g>') .. npairs.autopairs_cr()
        end
      else
        return npairs.autopairs_cr()
      end
    end
    remap('i', '<cr>', 'v:lua.MUtils.CR()', { expr = true, noremap = true })

    MUtils.BS = function()
      if vim.fn.pumvisible() ~= 0 and vim.fn.complete_info({ 'mode' }).mode == 'eval' then
        return npairs.esc('<C-e>') .. npairs.autopairs_bs()
      else
        return npairs.autopairs_bs()
      end
    end
    remap('i', '<BS>', 'v:lua.MUtils.BS()', { expr = true, noremap = true })
    --vim.api.nvim_del_keymap('n', '<c-h>')
  end

  keymaps()
end

return M
