" plugin/rc/devicons
scriptencoding utf-8


" loading the plugin
let g:webdevicons_enable = 1

" adding the flags to NERDTree
let g:webdevicons_enable_nerdtree = 1

" adding the custom source to unite
let g:webdevicons_enable_unite = 0

" adding the column to vimfiler
let g:webdevicons_enable_vimfiler = 0

" adding to vim-airline's tabline
let g:webdevicons_enable_airline_tabline = 0

" adding to vim-airline's statusline
let g:webdevicons_enable_airline_statusline = 0

" ctrlp glyphs
let g:webdevicons_enable_ctrlp = 0

" adding to vim-startify screen
let g:webdevicons_enable_startify = 0

" adding to flagship's statusline
let g:webdevicons_enable_flagship_statusline = 0

" turn on/off file node glyph decorations (not particularly useful)
let g:WebDevIconsUnicodeDecorateFileNodes = 1

" use double-width(1) or single-width(0) glyphs
" only manipulates padding, has no effect on terminal or set(guifont) font
"let g:WebDevIconsUnicodeGlyphDoubleWidth = 0

" whether or not to show the nerdtree brackets around flags
let g:webdevicons_conceal_nerdtree_brackets = 1

" the amount of space to use after the glyph character (default ' ')
"let g:WebDevIconsNerdTreeAfterGlyphPadding = ' '

" Force extra padding in NERDTree so that the filetype icons line up vertically
let g:WebDevIconsNerdTreeGitPluginForceVAlign = 1

" Adding the custom source to denite
let g:webdevicons_enable_denite = 1


if expand('$OSX') == 1
    let g:WebDevIconsOS = 'Darwin'
endif
