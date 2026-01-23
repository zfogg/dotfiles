-- rc/markdown

if vim.fn.has('nvim') == 1 then
  vim.g.markdown_fold_style = 'nested'
  vim.g.markdown_fold_override_foldtext = 0
end
