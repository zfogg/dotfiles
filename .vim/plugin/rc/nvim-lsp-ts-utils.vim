" plugin/rc/nvim-lsp-ts-utils
scriptencoding utf-8


if !PHas('nvim-lsp-ts-utils') | finish | endif


lua << EOF
-- enable null-ls integration (optional)
if 1 == vim.fn.PHas('null-ls.nvim') then
    local null_ls = require('null-ls')
    --null_ls.builtins.formatting.prettier,

    local sources = {
        null_ls.builtins.diagnostics.write_good,
        null_ls.builtins.code_actions.gitsigns,
        null_ls.builtins.diagnostics.shellcheck.with({
            diagnostics_format = '[#{c}] #{m} (#{s})'
        }),

        null_ls.builtins.formatting.eslint_d,
        null_ls.builtins.diagnostics.eslint_d,

        null_ls.builtins.formatting.clang_format,

        null_ls.builtins.formatting.golines,
        null_ls.builtins.formatting.goimports,
        null_ls.builtins.formatting.gofmt,

        null_ls.builtins.formatting.lua_format,
        null_ls.builtins.formatting.stylua,
        null_ls.builtins.diagnostics.luacheck,

        null_ls.builtins.diagnostics.pylint,

        null_ls.builtins.diagnostics.vint,

        null_ls.builtins.formatting.prismaFmt,

        null_ls.builtins.formatting.rustfmt,

        null_ls.builtins.formatting.sqlformat,

        null_ls.builtins.formatting.json_tool,
        null_ls.builtins.formatting.shfmt,
        null_ls.builtins.formatting.cmake_format,
        null_ls.builtins.formatting.nginx_beautifier,
    }
    null_ls.config({
        sources = sources,
    })
    --require('lspconfig')['null-ls'].setup{}
end
EOF
