-- lua/rc/coq.lua
local M = {}

local function keymaps()
  -- mapx removed, using native vim.keymap.set
end

function M.setup()
  -- Don't start COQ in headless mode
  if vim.fn.has('nvim') == 0 or vim.env.NVIM_HEADLESS or #vim.api.nvim_list_uis() == 0 then
    return
  end
  
  vim.g.coq_settings = vim.g.coq_settings or {}
  vim.g.coq_settings = vim.tbl_deep_extend('force', vim.g.coq_settings, {
    auto_start = 'shut-up',
    keymap = {
      recommended     = true,
      manual_complete = '<C-Space>',
      bigger_preview  = '<C-S-i>',
      jump_to_mark    = '<C-S-y>',
    },
    clients = {
      lsp         = {
        enabled = true,
        --resolve_timeout = 2,
        weight_adjust = 1.75,
      },
      tree_sitter = {
        enabled = true,
        weight_adjust = 1.5,
      },
      buffers     = { match_syms = true, },
      tmux        = { match_syms = false, },
      paths       = { preview_lines = 8, },
    },
  })
end

function M.config()
  -- Don't start COQ in headless mode
  if vim.fn.has('nvim') == 0 or vim.env.NVIM_HEADLESS or #vim.api.nvim_list_uis() == 0 then
    return
  end
  
  local coq = require "coq"
  --coq.Now() -- Start coq

  -- 3party sources
  --require("coq_3p")({
  --    { src = "nvimlua", short_name = "nLUA", conf_only = false }, -- Lua
  --    --{ src = "bc", short_name = "MATH", precision = 6 }, -- Calculator
  --    { src = "repl",
  --      sh = "zsh",
  --      shell = { p = "python", n = "node", },
  --      max_lines = 99,
  --      deadline = 500,
  --      unsafe = { "rm", "poweroff", "mv", },
  --    },
  --    { src = "copilot", short_name = "COP", accept_key = "<c-j>" },
  --  })

  keymaps()
end

return M
