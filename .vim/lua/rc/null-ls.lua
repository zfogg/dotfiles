-- lua/rc/null-ls.lua


-- enable null-ls integration (optional)
if 1 == vim.fn.PHas('null-ls.nvim') then
  local null_ls = require('null-ls').setup({
    --require("null-ls").builtins.formatting.prettier,
    require("null-ls").builtins.diagnostics.write_good,
    require("null-ls").builtins.code_actions.gitsigns,
    require("null-ls").builtins.diagnostics.shellcheck.with({
      diagnostics_format = '[#{c}] #{m} (#{s})'
    }),

    require("null-ls").builtins.formatting.eslint_d,
    require("null-ls").builtins.diagnostics.eslint_d,

    require("null-ls").builtins.formatting.clang_format,

    require("null-ls").builtins.formatting.golines,
    require("null-ls").builtins.formatting.goimports,
    require("null-ls").builtins.formatting.gofmt,

    require("null-ls").builtins.formatting.lua_format,
    require("null-ls").builtins.formatting.stylua,
    require("null-ls").builtins.diagnostics.luacheck,

    require("null-ls").builtins.diagnostics.pylint,

    require("null-ls").builtins.diagnostics.vint,

    require("null-ls").builtins.formatting.prismaFmt,

    require("null-ls").builtins.formatting.rustfmt,

    require("null-ls").builtins.formatting.sqlformat,

    require("null-ls").builtins.formatting.json_tool,
    require("null-ls").builtins.formatting.shfmt,
    require("null-ls").builtins.formatting.cmake_format,
    require("null-ls").builtins.formatting.nginx_beautifier,
  })
end
