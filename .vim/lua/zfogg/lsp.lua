-- lua/rc/lsp.lua

local servers = {
  'angularls',
  --'ansiblels',
  'bashls',
  --'omnisharp',
  'clangd',
  'cmake',
  'cssls',
  --'clojure_lsp',
  --'denols',
  'dockerls',
  --'efm',
  'eslintls',
  --'elixirls',
  --'elmls',
  --'ember',
  --'fortls',
  'gopls',
  'graphql',
  --'groovyls',
  'html',
  'hls',
  'jsonls',
  'jedi_language_server',
  --'kotlin_language_server',
  --'texlab',
  'sumneko_lua',
  --'intelephense',
  --'purescriptls',
  'pylsp',
  'pyright',
  --'rome',
  'solargraph',
  'rust_analyzer',
  'sqlls',
  'sqls',
  'stylelint_lsp',
  --'svelte',
  --'tailwindcss',
  --'terraformls',
  --'tflint',
  'tsserver',
  'vimls',
  --'vuels',
  'yamlls',
};

local a = require "plenary.async"
for _,v in pairs(servers) do
  require('nvim-lsp-installer').install(v)
end

return { servers = servers, };
