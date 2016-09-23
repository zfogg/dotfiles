" indentguides


let g:indent_guides_start_level           = 2
let g:indent_guides_guide_size            = 1
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_auto_colors           = 0


aug PluginConfigIndentGuides
    au!
    au VimEnter,Colorscheme * call z#indentguides#Highlight()
aug END
