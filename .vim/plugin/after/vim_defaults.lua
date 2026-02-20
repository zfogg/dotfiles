-- rc/vim_defaults

-- Disable provider support
vim.g.loaded_perl_provider = 0

-- Disable standard plugins
vim.g.loaded_getscriptPlugin = true
vim.g.loaded_gzip = true
vim.g.loaded_logipat = true
vim.g.loaded_rrhelper = true
vim.g.loaded_vimballPlugin = true

-- Disable netrw handlers
vim.g.loaded_netrwFileHandlers = true
vim.g.loaded_netrwSettings = true
vim.g.loaded_tar = true
vim.g.loaded_vimball = true
vim.g.loaded_xmlformat = true
vim.g.loaded_zip = true

-- Disable plugins that can slow things down
vim.g.loaded_netrwPlugin = true
vim.g.loaded_tarPlugin = true
vim.g.loaded_2html_plugin = true
vim.g.loaded_tutor_mode_plugin = true
vim.g.loaded_zipPlugin = true
vim.g.loaded_matchparen = true

-- Disable menu translation
vim.g.did_install_default_menus = true
vim.g.did_install_syntax_menu = true
vim.g.did_menu_trans = true
vim.g.menutrans_help_dialog = true
vim.g.menutrans_path_dialog = true
vim.g.menutrans_tags_dialog = true
vim.g.menutrans_textwidth_dialog = true
vim.g.menutrans_fileformat_dialog = true
vim.g.menutrans_fileformat_choices = true
vim.g.menutrans_set_lang_to = true
vim.g.menutrans_no_file = true

-- Configuration
local s_rc = {
  cursor_line = {
    auto = true,
    restore = true,
  }
}
