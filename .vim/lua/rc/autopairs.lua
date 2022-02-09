-- lua/rc/autopairs.lua
local npairs = require('nvim-autopairs')

npairs.setup({
  check_ts = true,
  map_bs = true,
  map_complete = true, -- it will auto insert `(` after select function or method item
  auto_select = true,  -- auto select first item
})

if 1 == vim.fn.PHas('coq_nvim') then
  local remap = vim.api.nvim_set_keymap
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
        -- you can change <c-g><c-g> to <c-e> if you don't use other i_CTRL-X modes
        return npairs.esc('<c-g><c-g>') .. npairs.autopairs_cr()
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
  --vim.api.nvim_del_keymap('n', '<c-h>')
end

if 1 == vim.fn.PHas('nvim-treesitter') then
  --local npairs = require('nvim-autopairs')
  --npairs.setup({
  --    check_ts = true,
  --    ts_config = {
  --        --lua = {'string'},-- it will not add pair on that treesitter node
  --        --javascript = {'template_string'},
  --        --java = false,-- don't check treesitter on java
  --    },
  --})

  require('nvim-treesitter.configs').setup {
    autopairs = {enable = true},
  }

  --local ts_conds = require('nvim-autopairs.ts-conds')
  -- press % => %% is only inside comment or string
  --npairs.add_rules({
  --  Rule("%", "%", "lua")
  --    :with_pair(ts_conds.is_ts_node({'string','comment'})),
  --  Rule("$", "$", "lua")
  --    :with_pair(ts_conds.is_not_ts_node({'function'}))
  --})
end
