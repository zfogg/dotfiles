" rc/colorsc
scriptencoding utf-8


" INFO: https://neovim.io/doc/user/syntax.html#g:vimsyn_embed
" lua python ruby
let g:vimsyn_embed = 'lPr'
let g:vimsyn_folding = 'aflPr'
let g:vimsyn_noerror = 1

if has('termguicolors') && has('nvim')
    set termguicolors
    set termbidi
    set guicursor=n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50
                \,a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor
                \,sm:block-blinkwait175-blinkoff150-blinkon175
endif

let g:base16colorspace  = get(g:, 'base16colorspace', 256)
if !exists('g:base16_shell_path')
  let g:base16_shell_path = get(environ(), 'BASE16_SHELL', expand('~/.config/base16-shell')).'/scripts'
endif

try
  source ~/.vimrc_background
catch /^Vim(colorscheme):E185/
  try
    colorscheme jellybeans
  catch /^Vim(colorscheme):E185/
    colorscheme desert
  endtry
endtry
