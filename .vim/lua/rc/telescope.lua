-- lua/rc/telescope.lua
local u = require('zfogg.util')

local M = {}

function M.config()
  local telescope = require('telescope')
  local actions = require("telescope.actions")
  telescope.setup({
      defaults = {
        vimgrep_arguments = { "rg",
          "--color=never", "--no-heading",
          "--with-filename", "--line-number",
          "--column", "--smart-case", "--trim"
        },
        --layout_strategy = 'flex',
        layout_strategy = 'flex',
        layout_config = {
          width = 0.65,
          width = 0.55,
          preview_cutoff = 40,
          prompt_position = "bottom",
        },
        scroll_strategy = 'cycle',
        color_devicons = true,
        mappings = {
          i = {
            ["<esc>"] = actions.close,
          },
        },
      },
      extensions = {
        --frecency = { workspaces = { exo = '/home/zfogg/projects/research/exoplanet' } },
        frecency = {
          show_scores    = true,
          show_unindexed = true,
          ignore_patterns = {"*.git/*", "*/tmp/*"}
        },
        fzf = {
          fuzzy     = true,
          case_mode = 'smart_case',
          override_generic_sorter = true,
          override_file_sorter    = true,
        },
      },
      pickers = {
        buffers = {
          previewer    = false,
          sort_lastused = true,
        },
        lsp_references      = { theme = 'dropdown', },
        lsp_code_actions    = { theme = 'dropdown', },
        lsp_definitions     = { theme = 'dropdown', },
        lsp_implementations = { theme = 'dropdown', },
      },
    })
end

local dropdown_preview = function(key)
  return require('telescope.themes').get_dropdown({
      winblend       = 20;
      layout_strategy = 'flex',
      layout_config = {
        width  = 0.75,
        height = 0.65,
        preview_cutoff = 120,
        prompt_position = "bottom",
      },
      show_line      = false;
      prompt_prefix  = '> ';
      prompt_title   = ''..key..'';
      borderchars = {
        prompt  = {'▀', '▐', '▄', '▌', '▛', '▜', '▟', '▙' };
        results = {'▀', '▐', '▄', '▌', '▛', '▜', '▟', '▙' };
        preview = {'▀', '▐', '▄', '▌', '▛', '▜', '▟', '▙' };
      };
    })
end
--local ts_opts = {theme = 'get_dropdown'}
local ts_opts = {}

local keymaps = function()
  local m = require('rc.mapx')
  local tsBuiltin = function(key, opts)
    if key == nil then error("choose a telescope.builtin key plz!") end
    opts = opts or {}
    return function(count)
      local p = dropdown_preview(key)
      require('telescope.builtin')[key](p)
    end
  end

  -- Navigate buffers and repos
  m.group('silent', { }, function()
    --nnoremap('<C-S-a>', ':Telescope buffers     show_all_buffers=true  theme=get_dropdown<CR>')
    --nnoremap('<C-t>t', tsBuiltin('planets',   {theme = 'get_dropdown'}))
    nnoremap('<C-t><C-t>',  tsBuiltin('builtin',    ts_opts))

    nnoremap('<C-t>t',  tsBuiltin('buffers',    u.tmerge(ts_opts, {show_all_buffers = true})))
    nnoremap('<C-t>',   tsBuiltin('buffers',    u.tmerge(ts_opts, {show_all_buffers = true})))

    nnoremap('<C-t>e',  function() require('telescope').extensions.frecency.frecency(dropdown_preview('frecency')) end)
    nnoremap('<C-e>',   function() require('telescope').extensions.frecency.frecency(dropdown_preview('frecency')) end)

    nnoremap('<C-t>g',  tsBuiltin('git_files',  ts_opts))
    nnoremap('<C-g>',   tsBuiltin('git_files',  ts_opts))

    nnoremap('<C-t>p',  tsBuiltin('find_files', ts_opts))
    nnoremap('<C-p>',   tsBuiltin('find_files', ts_opts))

    nnoremap('<C-t>f',  tsBuiltin('live_grep',  ts_opts))
    nnoremap('<C-f>',   tsBuiltin('live_grep',  ts_opts))
  end)
end

function M.setup()
  keymaps()
end

return M
