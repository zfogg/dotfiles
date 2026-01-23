-- plugin/after/plugins.lua
-- Load plugin-specific configurations after plugins are loaded

local after_dir = vim.fn.stdpath('config') .. '/plugin/after'

local plugin_configs = {
  'csv', 'undotree', 'typescript_vim', 'session', 'prettier',
  'nvim_scrollview', 'localvimrc', 'fzf', 'fugitive',
  'vim_syntax_expand', 'nerdcommenter', 'easymotion', 'quickscope',
  'vim_defaults', 'closetag', 'easyalign', 'highlightedyank',
  'incsearch', 'rust', 'repeat', 'go', 'markdown', 'stay',
  'neoterm', 'operator_flashy', 'racer', 'clang', 'vim_javascript',
  'cursorline', 'devicons', 'numberline', 'editorconfig', 'scriptnames',
  'syn', 'pgsql', 'fern_mappings'
}

for _, config in ipairs(plugin_configs) do
  local file = after_dir .. '/' .. config .. '.lua'
  local ok, err = pcall(dofile, file)
  if not ok then
    vim.notify('Failed to load ' .. config .. '.lua: ' .. err, vim.log.levels.WARN)
  end
end
