" after/plugin/vim_defaults
scriptencoding utf-8


" {{{ INFO: options to disable
let g:loaded_python_provider  = 0
"let g:loaded_python3_provider = 0
"let g:loaded_ruby_provider    = 0
"let g:loaded_node_provider    = 0
let g:loaded_perl_provider    = 0

" https://github.com/mhinz/vim-galore#standard-plugins
let g:loaded_getscriptPlugin  = v:true
let g:loaded_gzip             = v:true
let g:loaded_logipat          = v:true
let g:loaded_rrhelper         = v:true
"let g:loaded_spellfile_plugin = v:true
let g:loaded_vimballPlugin    = v:true

"let g:loaded_netrw                         = v:true              " $VIMRUNTIME/autoload/netrw.vim
"let g:loaded_netrwFileHandlers             = v:true              " $VIMRUNTIME/autoload/netrwFileHandlers.vim
"let g:loaded_netrwSettings                 = v:true              " $VIMRUNTIME/autoload/netrwSettings.vim
"let g:loaded_sql_completion                = 160                 " $VIMRUNTIME/autoload/sqlcomplete.vim
"let g:loaded_sql_completion                = v:true              " $VIMRUNTIME/autoload/sqlcomplete.vim
let g:loaded_tar                           = v:true              " $VIMRUNTIME/autoload/tar.vim
let g:loaded_vimball                       = v:true              " $VIMRUNTIME/autoload/vimball.vim
let g:loaded_xmlformat                     = v:true              " $VIMRUNTIME/autoload/xmlformat.vim
let g:loaded_zip                           = v:true              " $VIMRUNTIME/autoload/zip.vim
let loaded_less                            = v:true              " $VIMRUNTIME/macros/less.vim
let loaded_gzip                            = v:true              " $VIMRUNTIME/plugin/gzip.vim
let loaded_matchit                         = v:true              " $VIMRUNTIME/plugin/matchit.vim
"let g:loaded_matchparen                    = v:true              " $VIMRUNTIME/plugin/matchparen.vim
let g:loaded_matchparen                    = v:true                   " NOTE: https://github.com/itchyny/vim-parenmatch
"let g:loaded_netrwPlugin                   = '156'               " $VIMRUNTIME/plugin/netrwPlugin.vim
"let g:loaded_netrwPlugin                   = v:true
let loaded_rrhelper                        = v:true              " $VIMRUNTIME/plugin/rrhelper.vim
let loaded_spellfile_plugin                = v:true              " $VIMRUNTIME/plugin/spellfile.vim
"let g:loaded_tarPlugin                     = 'v29'               " $VIMRUNTIME/plugin/tarPlugin.vim
let g:loaded_tarPlugin                     = v:true
"let g:loaded_2html_plugin                  = 'vim7.4_v2'         " $VIMRUNTIME/plugin/tohtml.vim
let g:loaded_2html_plugin                  = v:true
let g:loaded_tutor_mode_plugin             = v:true              " $VIMRUNTIME/plugin/tutor.vim
"let g:loaded_zipPlugin                     = 'v28'               " $VIMRUNTIME/plugin/zipPlugin.vim
let g:loaded_zipPlugin                     = v:true
let myscriptsfile                          = v:true              " $VIMRUNTIME/scripts.vim
let did_install_default_menus              = v:true              " $VIMRUNTIME/menu.vim
let did_install_syntax_menu                = v:true              " $VIMRUNTIME/menu.vim
let did_menu_trans                         = v:true              " $VIMRUNTIME/menu.vim
let g:menutrans_help_dialog                = v:true              " $VIMRUNTIME/menu.vim
let g:menutrans_path_dialog                = v:true              " $VIMRUNTIME/menu.vim
let g:menutrans_tags_dialog                = v:true              " $VIMRUNTIME/menu.vim
let g:menutrans_textwidth_dialog           = v:true              " $VIMRUNTIME/menu.vim
let g:menutrans_fileformat_dialog          = v:true              " $VIMRUNTIME/menu.vim
let g:menutrans_fileformat_choices         = v:true              " $VIMRUNTIME/menu.vim
let g:ctags_command                        = v:true              " $VIMRUNTIME/menu.vim
let g:menutrans_set_lang_to                = v:true              " $VIMRUNTIME/menu.vim
let g:xxdprogram                           = v:true              " $VIMRUNTIME/menu.vim
let do_no_lazyload_menus                   = v:true              " $VIMRUNTIME/menu.vim
let no_buffers_menu                        = v:true              " $VIMRUNTIME/menu.vim
let bmenu_priority                         = v:true              " $VIMRUNTIME/menu.vim
let g:bmenu_max_pathlen                    = v:true              " $VIMRUNTIME/menu.vim
let g:menutrans_no_file                    = v:true              " $VIMRUNTIME/menu.vim
let g:menutrans_spell_change_ARG_to        = v:true              " $VIMRUNTIME/menu.vim
let g:menutrans_spell_add_ARG_to_word_list = v:true              " $VIMRUNTIME/menu.vim
let g:menutrans_spell_ignore_ARG           = v:true              " $VIMRUNTIME/menu.vim
" }}


" {{{ INFO: ~/.plugins/rc/
let s:rc = {}

" INFO: ~/.vim/plugin/rc/cursorline.vim
let s:rc.cursor_line = {
    \ 'auto': v:true,
    \ 'restore': v:true,
    \}
" }}
