" plugin/rc/nvim-compe
scriptencoding utf-8


if !has('nvim') | finish | endif
if !PHas('nvim-compe') | finish | endif


lua << EOF
if 1 == vim.fn.PHas('nvim-compe') then
  require'compe'.setup({
    enabled          = true;
    autocomplete     = true;
    debug            = false;
    min_length       = 1;
    preselect        = 'enable';
    throttle_time    = 80;
    source_timeout   = 200;
    resolve_timeout  = 800;
    incomplete_delay = 400;
    max_abbr_width   = 100;
    max_kind_width   = 100;
    max_menu_width   = 100;
    documentation = {
      border       = { '', '' ,'', ' ', '', '', '', ' ' }, -- the border option is the same as `|help nvim_open_win|`
      winhighlight = "NormalFloat:CompeDocumentation,FloatBorder:CompeDocumentationBorder",
      max_width    = 120,
      min_width    = 60,
      max_height   = math.floor(vim.o.lines * 0.3),
      min_height   = 1,
    };

    source = {
      path      = true;
      buffer    = true;
      tags      = false;
      spell     = false;
      calc      = false;
      omni      = true;
      emoji     = true;
      nvim_lsp  = true;
      nvim_lua  = true;
      -- 3rd-party plugins
      vsnip     = false;
      ultisnips = false;
      luasnip   = false;
      zsh        = 1 == vim.fn.PHas('compe-zsh');
      tmux       = 1 == vim.fn.PHas('compe-tmux');
      treesitter = 1 == vim.fn.PHas('nvim-treesitter');
      tabnine    = false;
      --tabnine   = {
      --  max_line                 = 1000;
      --  max_num_results          = 7;
      --  priority                 = 6000;
      --  -- setting sort to false means compe will leave tabnine to sort the completion items
      --  sort                     = true;
      --  show_prediction_strength = true;
      --  ignore_pattern           = '';
      --}; --1 == vim.fn.PHas('compe-tabnine');
    };
  })

  local t = function(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
  end

  local check_back_space = function()
      local col = vim.fn.col('.') - 1
      return col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') ~= nil
  end

  -- Use (s-)tab to:
  --- move to prev/next item in completion menuone
  --- jump to prev/next snippet's placeholder
  _G.tab_complete = function()
    if vim.fn.pumvisible() == 1 then
      return t "<C-n>"
    elseif check_back_space() then
      return t "<Tab>"
    else
      return vim.fn['compe#complete']()
    end
  end
  _G.s_tab_complete = function()
    if vim.fn.pumvisible() == 1 then
      return t "<C-p>"
    else
      -- If <S-Tab> is not working in your terminal, change it to <C-h>
      return t "<S-Tab>"
    end
  end

  vim.api.nvim_set_keymap("i", "<Tab>",     "v:lua.tab_complete()",   {expr = true})
  vim.api.nvim_set_keymap("s", "<Tab>",     "v:lua.tab_complete()",   {expr = true})
  vim.api.nvim_set_keymap("i", "<S-Tab>",   "v:lua.s_tab_complete()", {expr = true})
  vim.api.nvim_set_keymap("s", "<S-Tab>",   "v:lua.s_tab_complete()", {expr = true})

  vim.api.nvim_set_keymap("i", "<CR>",      "compe#confirm('<CR>')",  {expr = true})
  vim.api.nvim_set_keymap("i", "<C-Space>", "compe#complete()",       {expr = true })
end
EOF

"inoremap <silent><expr> <C-Space> compe#complete()
"inoremap <silent><expr> <CR>      compe#confirm(PHas('nvim-autopairs') ? luaeval("require 'nvim-autopairs'.autopairs_cr()") : '<CR>')

"inoremap <silent><expr> <C-e>     compe#close('<C-e>')
"inoremap <silent><expr> <C-f>     compe#scroll({ 'delta': +4 })
"inoremap <silent><expr> <C-d>     compe#scroll({ 'delta': -4 })

