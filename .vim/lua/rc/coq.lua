-- lua/rc/coq.lua
local M = {}

local function keymaps()
  local m = require('rc.mapx')
  --m.group("expr", { }, function()
  --end)
end

function M.setup()
  vim.g.coq_settings = vim.g.coq_settings or {}
  vim.g.coq_settings = vim.tbl_deep_extend('force', vim.g.coq_settings, {
      auto_start = 'shut-up';
      keymap = {
        recommended = true;
        manual_complete = '<C-Space>';
        bigger_preview  = '<C-S-i>';
        jump_to_mark    = '<C-S-y>';
      };
      clients = {
        buffers = { match_syms    = true; };
        tmux    = { match_syms    = false; };
        paths   = { preview_lines = 8; };
      };
    })
end

function M.config()
  local coq = require "coq"
  --coq.Now() -- Start coq

  -- 3party sources
  require("coq_3p")({
      { src = "nvimlua", short_name = "nLUA", conf_only = false }, -- Lua
      --{ src = "bc", short_name = "MATH", precision = 6 }, -- Calculator
      { src = "repl",
        sh = "zsh",
        shell = { p = "python", n = "node", },
        max_lines = 99,
        deadline = 500,
        unsafe = { "rm", "poweroff", "mv", },
      },
    })

  keymaps()
end

return M
