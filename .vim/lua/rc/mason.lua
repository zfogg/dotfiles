-- lua/rc/mason.lua
local util = require 'lspconfig.util'

local M = {}

local servers = {
  lua_ls = {
    Lua = {
      runtime = {
        -- Tell the language server which version of Lua you're using
        -- (most likely LuaJIT in the case of Neovim)
        version = 'LuaJIT',
      },
      -- Make the server aware of Neovim runtime files
      workspace = {
        checkThirdParty = true,
        library = {
          vim.env.VIMRUNTIME,
          "${3rd}/luv/library",
        },
        -- or pull in all of 'runtimepath'. NOTE: this is a lot slower
        -- library = vim.api.nvim_get_runtime_file("", true)
      },
      diagnostics = {
        enable = true,
        globals = {
          'vim',
          'PHas',
        },
      },
    },
    --root_dir = util.root_pattern(
    --  '.luarc.json',
    --  '.luarc.jsonc',
    --  '.luacheckrc',
    --  '.stylua.toml',
    --  'stylua.toml',
    --  'selene.toml',
    --  'selene.yml',
    --  '.git'
    --)
  },
  clangd = {},
  rust_analyzer = {},
  arduino_language_server = {},
  bashls = {},
  --cmake = {},
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
  tailwindcss = {},
  vtsls = {},
  solidity = {},
  yamlls = {},
  vimls = {},
}


function M.setup()
  vim.diagnostic.config({
    float = {
      scope = "cursor",
    },
    underline = true,
    virtual_text = {
      spacing = 4,
      source = "if_many",
      prefix = "■ ",
    },
    signs = true,
    update_in_insert = false,
    severity_sort = true,
  })
end

function M.config()
  require("mason").setup()

  -- Add Mason bin directory to PATH so LSP servers can be found
  local mason_bin = os.getenv("HOME") .. "/.local/share/nvim/mason/bin"
  vim.env.PATH = mason_bin .. ":" .. vim.env.PATH

  -- Get capabilities from blink.cmp
  local capabilities = require('blink.cmp').get_lsp_capabilities()

  -- Add additional codeAction capabilities
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

  require("lsp-format").setup({})

  --  This function gets run when an LSP connects to a particular buffer.
  local on_attach = function(client, bufnr)
    local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
    local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

    require("lsp-format").on_attach(client, bufnr)

    require("lsp_signature").on_attach({
      bind = true,
      handler_opts = {
        border = "rounded",
      },
    }, bufnr)
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

    nmap('gd', function() require('telescope.builtin').lsp_definitions() end, '[g]oto [d]efinition')
    nmap('gr', function() require('telescope.builtin').lsp_references() end, '[g]oto [r]eferences')
    nmap('gI', function() require('telescope.builtin').lsp_implementations() end, '[G]oto [I]mplementation')
    nmap('<leader>D', function() require('telescope.builtin').lsp_type_definitions() end, 'Type [D]efinition')
    nmap('<leader>ds', function() require('telescope.builtin').lsp_document_symbols() end, '[D]ocument [S]ymbols')
    nmap('<leader>ws', function() require('telescope.builtin').lsp_dynamic_workspace_symbols() end,
      '[W]orkspace [S]ymbols')

    -- See `:help K` for why this keymap
    nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
    --nmap('<leader><leader>', vim.lsp.buf.signature_help, 'Signature Documentation')

    vim.keymap.set({ 'n' }, '<leader><c-k>', function()
      require('lsp_signature').toggle_float_win()
    end, { silent = true, noremap = true, desc = 'toggle signature' })
    vim.keymap.set({ 'i' }, '<c-k>', function()
      require('lsp_signature').toggle_float_win()
    end, { silent = true, noremap = true, desc = 'toggle signature' })

    vim.keymap.set({ 'n' }, '<Leader><leader>', function()
      vim.lsp.buf.signature_help()
    end, { silent = true, noremap = true, desc = 'toggle signature' })

    -- Set some keybinds conditional on server capabilities
    if client.server_capabilities.document_formatting then
      buf_set_keymap("n", "<leader>lf",
        "<cmd>lua vim.lsp.buf.formatting()<CR>", {})
    elseif client.server_capabilities.document_range_formatting then
      buf_set_keymap("n", "<leader>lf",
        "<cmd>lua vim.lsp.buf.range_formatting()<CR>", {})
    end

    vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
    vim.keymap.set('n', ']d', vim.diagnostic.goto_next)

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

    --vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'
    vim.api.nvim_command('setlocal omnifunc=v:lua.vim.lsp.omnifunc')
    vim.api.nvim_exec_autocmds('User', { pattern = 'LspAttached' })
  end

  -- Use only the new vim.lsp.config API

  -- Configure clangd with custom capabilities
  vim.lsp.config('clangd', {
    capabilities = capabilities,
  })
  vim.lsp.enable('clangd')

  -- Configure other servers
  for server_name, server_config in pairs(servers) do
    -- Get the default config from lspconfig if available
    local ok, lspconfig_spec = pcall(function()
      return require('lspconfig.util').available_servers()[server_name]
    end)

    local opts = {
      capabilities = capabilities,
    }

    -- Add settings if provided (only if there's actual content besides cmd)
    for k, v in pairs(server_config) do
      if k ~= 'cmd' then
        if not opts.settings then opts.settings = {} end
        opts.settings[k] = v
      end
    end

    -- Add cmd if provided
    if server_config.cmd then
      opts.cmd = server_config.cmd
    end

    vim.lsp.config(server_name, opts)
    vim.lsp.enable(server_name)
  end

  -- Setup an LspAttach autocommand to call our on_attach for all servers
  vim.api.nvim_create_autocmd('LspAttach', {
    callback = function(args)
      local client_id = args.data.client_id
      local bufnr = args.buf
      on_attach(vim.lsp.get_client_by_id(client_id), bufnr)
    end,
  })

  -- Don't use mason-lspconfig integration - we configure servers manually with vim.lsp.config
end

return M
