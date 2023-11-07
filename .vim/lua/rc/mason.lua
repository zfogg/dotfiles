-- lua/rc/mason.lua

local M = {}

local servers = {
  lua_ls = {},
  rust_analyzer = {},
  arduino_language_server = {},
  bashls = {},
  clangd = {},
  cmake = {},
  --neocmake = {},
  cssls = {},
  --gopls = {},
  graphql = {},
  html = {},
  --hls = {},
  jsonls = {},
  --tsserver = {},
  quick_lint_js = {},
  marksman = {},
  jedi_language_server = {},
  pylsp = {},
  ruby_ls = {},
  tailwindcss = {},
  vtsls = {},
  solidity = {},
  yamlls = {},
  vimls = {},
}

function M.setup()
  vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
      underline        = true,
      signs            = true,
      update_in_insert = false,
      severity_sort    = true,
      virtual_text     = false,
      --virtual_text = {
        --prefix  = "",
        --spacing = 2,
      --},
  })

  vim.diagnostic.config({
    float = {
      scope = "cursor",
    },
    underline = false,
    virtual_text = false,
    signs = true,
    update_in_insert = false,
    severity_sort = true,
  })
end

function M.config()
  require("mason").setup()

  -- Ensure the servers above are installed
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities.textDocument.codeAction = {
    dynamicRegistration = true,
    codeActionLiteralSupport = {
      codeActionKind = {
        valueSet = (function()
          local res = vim.tbl_values(vim.lsp.protocol.CodeActionKind)
          table.sort(res)
          return res
        end)()
      }
    }
  }

  capabilities.textDocument.completion.completionItem.documentationFormat     = { 'markdown', 'plaintext' }
  capabilities.textDocument.completion.completionItem.snippetSupport          = true
  capabilities.textDocument.completion.completionItem.preselectSupport        = true
  capabilities.textDocument.completion.completionItem.insertReplaceSupport    = true
  capabilities.textDocument.completion.completionItem.labelDetailsSupport     = true
  capabilities.textDocument.completion.completionItem.deprecatedSupport       = true
  capabilities.textDocument.completion.completionItem.commitCharactersSupport = true
  capabilities.textDocument.completion.completionItem.tagSupport              = { valueSet = { 1 } }
  capabilities.textDocument.completion.completionItem.resolveSupport          = {
    properties = {
      'documentation',
      'detail',
      'additionalTextEdits',
    },
  }
  --  This function gets run when an LSP connects to a particular buffer.
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end
  local on_attach = function(client, bufnr)
    require("lsp_signature").on_attach({
        bind = true,
        handler_opts = {
          border = "rounded",
        },
      })
    -- NOTE: Remember that lua is a real programming language, and as such it is possible
    -- to define small helper and utility functions so you don't have to repeat yourself
    -- many times.
    --
    -- In this case, we create a function that lets us more easily define mappings specific
    -- for LSP related items. It sets the mode, buffer and description for us each time.
    local nmap = function(keys, func, desc)
      if desc then
        desc = 'LSP: ' .. desc
      end

      vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
    end

    nmap('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction')
    nmap('<leader>rn', vim.lsp.buf.rename, '[r]e[n]ame')

    nmap('gD', vim.lsp.buf.declaration, '[g]oto [D]eclaration')

    nmap('gd', require('telescope.builtin').lsp_definitions, '[g]oto [d]efinition')
    nmap('gr', require('telescope.builtin').lsp_references, '[g]oto [r]eferences')
    nmap('gI', require('telescope.builtin').lsp_implementations, '[G]oto [I]mplementation')
    nmap('<leader>D', require('telescope.builtin').lsp_type_definitions, 'Type [D]efinition')
    nmap('<leader>ds', require('telescope.builtin').lsp_document_symbols, '[D]ocument [S]ymbols')
    nmap('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')

    -- See `:help K` for why this keymap
    nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
    --nmap('<leader><leader>', vim.lsp.buf.signature_help, 'Signature Documentation')

    vim.keymap.set({ 'n' }, '<leader>K', function()
      require('lsp_signature').toggle_float_win()
    end, { silent = true, noremap = true, desc = 'toggle signature' })

    vim.keymap.set({ 'n' }, '<Leader><leader>', function()
      vim.lsp.buf.signature_help()
    end, { silent = true, noremap = true, desc = 'toggle signature' })

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

    vim.api.nvim_command('setlocal omnifunc=v:lua.vim.lsp.omnifunc')
    vim.api.nvim_exec_autocmds('User', {pattern = 'LspAttached'})
  end

  local mason_lspconfig = require 'mason-lspconfig'
  local coq = require('coq')
  local lspconfig = require('lspconfig')
  mason_lspconfig.setup {
    ensure_installed = vim.tbl_keys(servers),
    handlers = {
      function(server_name)
        lspconfig[server_name].setup({
            capabilities = capabilities,
            on_attach = on_attach,
            settings = servers[server_name],
            filetypes = (servers[server_name] or {}).filetypes,
          })
      end,
    },
  }
end

return M
