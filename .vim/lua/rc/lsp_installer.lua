-- lua/rc/telescope.lua
--local lsp           = require'lspconfig'
--local configs       = require'lspconfig/configs'
--local util          = require'lspconfig/util'

--require('lang.keymappings')

function _G.HoverFixed()
  --vim.api.nvim_command('set eventignore=CursorHold')
  --vim.lsp.buf.hover()
  --vim.api.nvim_command('au CursorMoved <buffer> ++once set eventignore=""')
  --vim.lsp.buf.hover()
  --vim.diagnostic.open_float()
  --local bfrn = vim.api.nvim_buf_get_number(0)
  --local row = vim.fn.line('.')
  --local col = vim.fn.col('.')
end

-- Show diagnostics in a pop-up window on hover
_G.LspDiagnosticsPopupHandler = function()
  local current_cursor = vim.api.nvim_win_get_cursor(0)
  local last_popup_cursor = vim.w.lsp_diagnostics_last_cursor or {nil, nil}
  -- Show the popup diagnostics window,
  -- but only once for the current cursor location (unless moved afterwards).
  if not (current_cursor[1] == last_popup_cursor[1] and current_cursor[2] == last_popup_cursor[2]) then
    vim.w.lsp_diagnostics_last_cursor = current_cursor
    if #vim.diagnostic.get() > 0 then
    --if #vim.diagnostic.get(0, {lnum = vim.fn.line('.')+1}) > 0 then
      vim.diagnostic.open_float(0, {scope="cursor"})   -- for neovim 0.6.0+, replaces show_{line,position}_diagnostics
    else
      vim.lsp.buf.hover()
    end
  end
end

local function common_on_attach(client, bufnr)
  vim.api.nvim_exec_autocmds('User', {pattern = 'LspAttached'})
  -- ... set up buffer keymaps, etc.

  local function buf_set_keymap(...)
    vim.api.nvim_buf_set_keymap(bufnr, ...)
  end
  local function buf_set_option(...)
    vim.api.nvim_buf_set_option(bufnr, ...)
  end

  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')
  --vim.api.nvim_command('au CursorHold * lua _G.HoverFixed()')
  vim.cmd [[
  augroup LSPDiagnosticsOnHover
    autocmd!
    "autocmd CursorHold * lua _G.LspDiagnosticsPopupHandler()
  augroup END
  ]]

  -- Mappings.
  local opts = {noremap = true, silent = true}
  buf_set_keymap('n', 'gD',          '<Cmd>lua vim.lsp.buf.declaration()<CR>',                                opts)
  buf_set_keymap('n', 'gd',          '<Cmd>lua vim.lsp.buf.definition()<CR>',                                 opts)
  buf_set_keymap('n', 'K',           '<Cmd>lua vim.lsp.buf.hover()<CR>',                                      opts)
  buf_set_keymap('n', 'gi',          '<cmd>lua vim.lsp.buf.implementation()<CR>',                             opts)
  buf_set_keymap('n', '<C-S-k>',     '<cmd>lua vim.lsp.buf.signature_help()<CR>',                             opts)
  buf_set_keymap('n', '[d',          '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>',                           opts)
  buf_set_keymap('n', ']d',          '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>',                           opts)
  buf_set_keymap('n', '<leader>law', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>',                       opts)
  buf_set_keymap('n', '<leader>lrw', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>',                    opts)
  buf_set_keymap('n', '<leader>llw', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<leader>lt',  '<cmd>lua vim.lsp.buf.type_definition()<CR>',                            opts)
  buf_set_keymap('n', '<leader>lrn', '<cmd>lua vim.lsp.buf.rename()<CR>',                                     opts)
  buf_set_keymap('n', '<leader>lrf', '<cmd>lua vim.lsp.buf.references()<CR>',                                 opts)
  buf_set_keymap('n', '<leader>ld',  '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>',               opts)
  buf_set_keymap('n', '<leader>ll',  '<cmd>lua vim.lsp.diagnostic.setloclist()<CR>',                          opts)
  buf_set_keymap('n', '<leader>lca', '<cmd>lua vim.lsp.buf.code_action()<CR>',                                opts)

  -- Set some keybinds conditional on server capabilities
  if client.server_capabilities.document_formatting then
    buf_set_keymap("n", "<leader>lf",
    "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
  elseif client.server_capabilities.document_range_formatting then
    buf_set_keymap("n", "<leader>lf",
    "<cmd>lua vim.lsp.buf.range_formatting()<CR>", opts)
  end

  -- Set autocommands conditional on server_capabilities
  if client.server_capabilities.document_highlight then
    vim.api.nvim_exec([[
      hi LspReferenceRead  cterm=bold ctermbg=red guibg=LightYellow
      hi LspReferenceText  cterm=bold ctermbg=red guibg=LightYellow
      hi LspReferenceWrite cterm=bold ctermbg=red guibg=LightYellow
      augroup lsp_document_highlight
        autocmd! * <buffer>
        "autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
    ]], false)
  end

  -- INFO: https://github.com/jubnzv/virtual-types.nvim
  --if vim.fn.PHas('virtualtypes.nvim') then
    --require('virtualtypes').on_attach(client, bufnr)
  --end
end


-- symbols-outline.nvim
vim.g.symbols_outline = {
  highlight_hovered_item = true,
  show_guides = true,
  --auto_preview = false, -- experimental
  auto_preview = true, -- experimental
  position = 'right',
  keymaps = {
    close          = "<Esc>",
    goto_location  = "<Cr>",
    focus_location = "o",
    hover_symbol   = "<C-space>",
    rename_symbol  = "r",
    code_actions   = "a"
  },
  lsp_blacklist = {}
}


-- Send diagnostics to quickfix list
do
  -- INFO: set a handeler for "textDocument/publishDiagnostics" BEFORE the quickfix/locationlist handler
  vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
      underline        = true,
      signs            = true,
      update_in_insert = false,
      virtual_text     = false,
      --virtual_text = {
        --prefix  = "",
        --spacing = 4,
      --},
  })
end

vim.fn.sign_define("LspDiagnosticsSignError", {
  text = "",
  texthl = "LspDiagnosticsSignError"
})
vim.fn.sign_define("LspDiagnosticsSignWarning", {
  text = "",
  texthl = "LspDiagnosticsSignWarning"
})
vim.fn.sign_define("LspDiagnosticsSignInformation", {
  text = "",
  texthl = "LspDiagnosticsSignInformation"
})
vim.fn.sign_define("LspDiagnosticsSignHint", {
  text = "➤",
  texthl = "LspDiagnosticsSignHint"
})

vim.o.shortmess = vim.o.shortmess .. "c"

