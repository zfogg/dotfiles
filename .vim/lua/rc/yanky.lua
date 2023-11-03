-- lua/rc/yanky.lua

if 1 == vim.fn.PHas('yanky.nvim') then
  local utils = require("yanky.utils")
  local mapping = require("yanky.telescope.mapping")

  require("yanky").setup({
    ring = {
      history_length = 100,
      storage = "shada",
      sync_with_numbered_registers = true,
      cancel_event = "update",
    },
    system_clipboard = {
      sync_with_ring = true,
    },
    preserve_cursor_position = {
      enabled = true,
    },
    highlight = {
      on_put = true,
      on_yank = true,
      timer = 500,
    },
    picker = {
      select = {
        action = nil, -- nil to use default put action
      },
      telescope = {
        default = mapping.put("p"),
        i = {
          --["<c-p>"] = mapping.put("p"),
          --["<s-Enter>"] = mapping.put("P"),
          ["<c-x>"] = mapping.delete(),
          ["<c-r>"] = mapping.set_register(utils.get_default_register()),
        },
        n = {
          p = mapping.put("p"),
          P = mapping.put("P"),
          d = mapping.delete(),
          r = mapping.set_register(utils.get_default_register()),
        },
      },
    },
  })

  -- Preserve cursor position.
  vim.keymap.set({"n","x"}, "y", "<Plug>(YankyYank)")

  vim.keymap.set({"n","x"}, "p", "<Plug>(YankyPutAfter)")
  vim.keymap.set({"n","x"}, "P", "<Plug>(YankyPutBefore)")
  vim.keymap.set({"n","x"}, "gp", "<Plug>(YankyGPutAfter)")
  vim.keymap.set({"n","x"}, "gP", "<Plug>(YankyGPutBefore)")
  vim.keymap.set("n", "<c-y><c-n>", "<Plug>(YankyCycleForward)")
  vim.keymap.set("n", "<c-y><c-p>", "<Plug>(YankyCycleBackward)")

  vim.keymap.set("n", "]p", "<Plug>(YankyPutIndentAfterLinewise)")

  vim.keymap.set("n", "[p", "<Plug>(YankyPutIndentBeforeLinewise)")
  vim.keymap.set("n", "]P", "<Plug>(YankyPutIndentAfterLinewise)")
  vim.keymap.set("n", "[P", "<Plug>(YankyPutIndentBeforeLinewise)")

  vim.keymap.set("n", ">p", "<Plug>(YankyPutIndentAfterShiftRight)")
  vim.keymap.set("n", "<p", "<Plug>(YankyPutIndentAfterShiftLeft)")
  vim.keymap.set("n", ">P", "<Plug>(YankyPutIndentBeforeShiftRight)")
  vim.keymap.set("n", "<P", "<Plug>(YankyPutIndentBeforeShiftLeft)")

  vim.keymap.set("n", "=p", "<Plug>(YankyPutAfterFilter)")
  vim.keymap.set("n", "=P", "<Plug>(YankyPutBeforeFilter)")
end
