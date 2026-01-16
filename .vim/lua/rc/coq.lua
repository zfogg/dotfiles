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

  if vim.fn.PHas('nvim-autopairs') == 1 then
    local remap = vim.api.nvim_set_keymap
    local npairs = require('nvim-autopairs')

    npairs.setup({ map_bs = false, map_cr = false })

    vim.g.coq_settings = { keymap = { recommended = false } }

    -- these mappings are coq recommended mappings unrelated to nvim-autopairs
    remap('i', '<esc>', [[pumvisible() ? "<c-e><esc>" : "<esc>"]], { expr = true, noremap = true })
    remap('i', '<c-c>', [[pumvisible() ? "<c-e><c-c>" : "<c-c>"]], { expr = true, noremap = true })
    remap('i', '<tab>', [[pumvisible() ? "<c-n>" : "<tab>"]], { expr = true, noremap = true })
    remap('i', '<s-tab>', [[pumvisible() ? "<c-p>" : "<bs>"]], { expr = true, noremap = true })

    -- skip it, if you use another global object
    _G.MUtils= {}

    MUtils.CR = function()
      if vim.fn.pumvisible() ~= 0 then
        if vim.fn.complete_info({ 'selected' }).selected ~= -1 then
          return npairs.esc('<c-y>')
        else
          return npairs.esc('<c-e>') .. npairs.autopairs_cr()
        end
      else
        return npairs.autopairs_cr()
      end
    end
    remap('i', '<cr>', 'v:lua.MUtils.CR()', { expr = true, noremap = true })

    MUtils.BS = function()
      if vim.fn.pumvisible() ~= 0 and vim.fn.complete_info({ 'mode' }).mode == 'eval' then
        return npairs.esc('<c-e>') .. npairs.autopairs_bs()
      else
        return npairs.autopairs_bs()
      end
    end
    remap('i', '<bs>', 'v:lua.MUtils.BS()', { expr = true, noremap = true })
  end -- has('nvim-autopairs')
end

return M
