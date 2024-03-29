-- lua/rc/lsp_signature.lua

local M = {}


function M.setup()
end

local floating_window_off_y = function() -- adjust float windows y position. e.g. set to -2 can make floating window move up 2 lines
  local linenr = vim.api.nvim_win_get_cursor(0)[1] -- buf line number
  local pumheight = vim.o.pumheight
  local winline = vim.fn.winline() -- line number in the window
  local winheight = vim.fn.winheight(0)

  -- window top
  if winline - 1 < pumheight then
    return pumheight
  end

  -- window bottom
  if winheight - winline < pumheight then
    return -pumheight
  end
  return 0
end



function M.config()
  require "lsp_signature".setup({
      --bind = true, -- This is mandatory, otherwise border config won't get registered.
      bind = true, -- This is mandatory, otherwise border config won't get registered.
      -- If you want to hook lspsaga or other signature handler, pls set to false
      doc_lines = 3, -- will show two lines of comment/doc(if there are more than two lines in doc, will be truncated);
      -- set to 0 if you DO NOT want any API comments be shown
      -- This setting only take effect in insert mode, it does not affect signature help in normal
      -- mode, 10 by default

      --floating_window = false, -- show hint in a floating window, set to false for virtual text only mode
      floating_window = false, -- show hint in a floating window, set to false for virtual text only mode
      floating_window_above_cur_line = true, -- try to place the floating above the current line when possible Note:
      floating_window_off_x = 1, -- adjust float windows x position.
      --floating_window_off_y = -2, -- adjust float windows y position.
      floating_window_off_y = floating_window_off_y, -- adjust float windows y position.
      -- will set to true when fully tested, set to false will use whichever side has more space
      -- this setting will be helpful if you do not want the PUM and floating win overlap
      fix_pos = false,  -- set to true, the floating window will not auto-close until finish all parameters
      --hint_enable = false, -- virtual hint enable
      hint_enable = true, -- virtual hint enable
      hint_prefix = "❕ ",  -- Panda for parameter
      --hint_prefix = "",  -- Nothing for nothing
      hint_scheme = "String",
      use_lspsaga = false,  -- set to true if you want to use lspsaga popup
      hi_parameter = "LspSignatureActiveParameter", -- how your parameter will be highlight
      max_height = 12, -- max height of signature floating_window, if content is more than max_height, you can scroll down
      -- to view the hiding contents
      max_width = 80,  -- max_width of signature floating_window, line will be wrapped if exceed max_width
      handler_opts = {
        border = "shadow"   -- double, single, shadow, none
      },

      auto_close_after = nil, -- autoclose signature float win after x sec, disabled if nil.
      trigger_on_newline = false, -- set to true if you need multiple line parameter, sometime show signature on new line can be confusing, set it to false for #58
      extra_trigger_chars = {}, -- Array of extra characters that will trigger signature completion, e.g., {"(", ","}
      -- deprecate !!
      -- decorator = {"`", "`"}  -- this is no longer needed as nvim give me a handler and it allow me to highlight active parameter in floating_window
      zindex = 200, -- by default it will be on top of all floating windows, set to 50 send it to bottom
      debug = false, -- set to true to enable debug logging
      log_path = "debug_log_file_path", -- debug log path

      padding = ' ', -- character to pad on left and right of signature can be ' ', or '|'  etc

      transparency = 30,
      shadow_blend = 36, -- if you using shadow as border use this set the opacity
      shadow_guibg = 'Black', -- if you using shadow as border use this set the color e.g. 'Green' or '#121315'
      timer_interval = 200,
      toggle_key = '<C-k>' -- toggle signature on and off in insert mode,  e.g. toggle_key = '<M-x>'
    })
end


return M
