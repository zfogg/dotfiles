" plugin/rc/nvim-lspconfig
scriptencoding utf-8

if !has('nvim') || !PHas('nvim-lspconfig') | finish | endif


lua << EOF
local nvim_lsp = require('lspconfig')

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

nvim_lsp.bashls.setup{}
nvim_lsp.denols.setup{}
nvim_lsp.diagnosticls.setup{}
nvim_lsp.dockerls.setup{}
nvim_lsp.graphql.setup{}
nvim_lsp.jedi_language_server.setup{}
nvim_lsp.jsonls.setup{}
nvim_lsp.pylsp.setup{}
nvim_lsp.tsserver.setup{}
nvim_lsp.vimls.setup{}
nvim_lsp.yamlls.setup{}
nvim_lsp.stylelint_lsp.setup{}
--nvim_lsp.terraformls.setup{}
nvim_lsp.cmake.setup{}
nvim_lsp.rust_analyzer.setup{}
--nvim_lsp.rnix.setup{}
nvim_lsp.pyright.setup{}
nvim_lsp.solargraph.setup{}

--Enable (broadcasting) snippet capability for completion
nvim_lsp.cssls.setup { capabilities = capabilities, }
nvim_lsp.html.setup  { capabilities = capabilities, }

nvim_lsp.rls.setup {
  settings = {
    rust = {
      unstable_features = true,
      build_on_save = false,
      all_features = true,
    },
  },
}
EOF

