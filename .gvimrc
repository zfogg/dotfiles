" ==========================================================
" .gvimrc | Vim 7.4 {{{1
" ==========================================================

" Author: Zach Fogg - zach@zachfogg.com

" }}}1


" ==========================================================
" Settings {{{1
" ==========================================================

try
    colorscheme base16-default
catch /^Vim\%((\a\+)\)\=:E185/
    colorscheme jellybeans
endtry

" Don't popup a menu on any <a-{x}>.
set winaltkeys=no

" Remove menu garbage from gVim's GUI.
set guioptions=ac

" }}}1


" ==========================================================
" OS Settings {{{1
" ==========================================================

if has("win32")
  set guifont=Consolas:h11
elseif has("unix")
  set guifont=DejaVu\ Sans\ Mono\ for\ Powerline\ 16
endif

" Borders match colorscheme when fullscreen.
if has('gui_gtk') && has('gui_running')
  let s:border = synIDattr(synIDtrans(hlID('Normal')), 'bg', 'gui')
  exe 'silent !echo ''style "vimfix" { bg[NORMAL] = "' . escape(s:border, '#') . '" }'''.
        \' > ~/.gtkrc-2.0'
  exe 'silent !echo ''widget "vim-main-window.*GtkForm" style "vimfix"'''.
        \' >> ~/.gtkrc-2.0'
endif

" }}}1


" ==========================================================
" Keybindings {{{1
" ==========================================================

" For embedded GVim in Eclipse.
inoremap <C-C> <Esc>

" }}}1

