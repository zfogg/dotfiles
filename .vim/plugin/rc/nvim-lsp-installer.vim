" plugin/rc/nvim-lsp-installer
scriptencoding utf-8


if !has('nvim') | finish | endif
if !PHas('nvim-lsp-installer') | finish | endif


lua << EOF
local lsp_installer = require'nvim-lsp-installer'
local lsp           = require'lspconfig'
local configs       = require'lspconfig/configs'
local util          = require'lspconfig/util'

function common_on_attach(client, bufnr)
  -- ... set up buffer keymaps, etc.
end

lsp_installer.on_server_ready(function(server)
  local opts = {
    on_attach = common_on_attach
  };

  if server.name == 'eslintls' then
    --configs[server.name] = {
      --default_config = {
      --cmd = {'eslint-ls', '--stdio'};
      opts.settings = {
        format = { enable = true };
      };
      opts.filetypes = {
        'javascript',
        'javascriptreact',
        'javascript.jsx',
        'typescript',
        'typescriptreact',
        'typescript.tsx'
      };
      opts.root_dir = util.root_pattern(
        '.eslintrc',
        '.eslintrc.js',
        '.eslintrc.json',
        '.eslintrc.yaml',
        '.eslintignore',
        'package.json',
        '.git'
      );
  end

  server:setup(opts)

  if 1 == vim.fn.PHas('coq_nvim') then
    server:setup(coq.lsp_ensure_capabilities(opts))
  end

  vim.cmd [[ do User LspAttachBuffers ]]
end)
EOF
