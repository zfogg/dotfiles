-- lua/rc/telescope.lua
local u = require('zfogg.util')

local M = {}

function M.config()
  local telescope = require('telescope')
  local actions = require("telescope.actions")
  telescope.setup({
      defaults = {
        dynamic_preview_title = true,
        vimgrep_arguments = { "rg",
          "--color=never", "--no-heading",
          "--with-filename", "--line-number",
          "--column", "--smart-case", "--trim"
        },
        --layout_strategy = 'flex',
        layout_strategy = 'flex',
        layout_config = {
          width = 0.65,
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
          ignore_patterns = {"*.git/*", "*/tmp/*"},
          workspaces = {
            ["vim"]           = vim.env.HOME.."/.vim",
            ["zsh"]           = vim.env.HOME.."/.config/zsh",
            ["bao-ui"]        = vim.env.HOME.."/src/github.com/baofinance/bao-ui",
            ["bao-contracts"] = vim.env.HOME.."/src/github.com/baofinance/bao-contracts",
            ["bao-distr"]     = vim.env.HOME.."/src/github.com/baofinance/bao-distribution",
          },
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

   --require('telescope').load_extension('fzf')
   --require('telescope').load_extension('frecency')
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
    local preview = dropdown_preview(key)
    opts = vim.tbl_extend("force", preview, opts)
    --return function(count)
    return function()
      require('telescope.builtin')[key](opts)
    end
  end

  local n_keymap = function(lhs, rhs)
    vim.api.nvim_set_keymap('n', lhs, rhs, { noremap = true, silent = true })
  end

  -- Navigate buffers and repos
  m.group('silent', { }, function()
    --m.nnoremap('<C-S-a>', ':Telescope buffers     show_all_buffers=true  theme=get_dropdown<CR>')
    --m.nnoremap('<C-t>t', tsBuiltin('planets',   {theme = 'get_dropdown'}))
    m.nnoremap('<C-t><C-t>',  tsBuiltin('builtin',    ts_opts))

    --m.nnoremap('<C-t>t',  tsBuiltin('buffers',    u.tmerge(ts_opts, {show_all_buffers = true})))
    m.nnoremap('<C-t>b',  tsBuiltin('buffers',    u.tmerge(ts_opts, {show_all_buffers = true})))

    m.nnoremap('<C-t>g',  tsBuiltin('git_files',  ts_opts))
    --m.nnoremap('<C-g>',   tsBuiltin('git_files',  ts_opts))

    m.nnoremap('<C-t>p',  function() require('telescope').extensions.frecency.frecency({ workspace = 'CWD' }) end)
    m.nnoremap('<C-p>',   function() require('telescope').extensions.frecency.frecency({ workspace = 'CWD' }) end)
    --m.nnoremap('<C-t>p',  tsBuiltin('find_files', ts_opts))
    --m.nnoremap('<C-p>',   tsBuiltin('find_files', ts_opts))

    m.nnoremap('<C-t>f',  tsBuiltin('live_grep',  ts_opts))
    m.nnoremap('<C-f>',   tsBuiltin('live_grep',  ts_opts))

    m.nnoremap('<C-t>l',  tsBuiltin('diagnostics',  ts_opts))

    m.nnoremap('<C-t><space>',  function() require('telescope').extensions.emoji.emoji() end)

    -- INFO: uninstalled
    --m.nnoremap('<C-t>y',  function() vim.cmd('Telescope yank_history') end)
    --m.nnoremap('<C-y>',   function() vim.cmd('Telescope yank_history') end)
  end)
end

function M.setup()
  keymaps()
end

return M
