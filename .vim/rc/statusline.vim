" rc/settings
scriptencoding utf-8


" requires Powerline font
set statusline=

" buffer number
set statusline+=%(\ %{'help'!=&filetype?bufnr('%'):''}\ \ %)

" filename
set statusline+=%(%{substitute(expand('%:h:t').'/'.expand('%:t'),'^\.\/','','')}\ %)

" buffer
set statusline+=%{&modified?'\ +':''}
set statusline+=%{&readonly?'\ ':''}

set statusline+=\ %1*%=

" file type and format
set statusline+=\ %{''!=#&filetype?&filetype:'none'}
set statusline+=%(\ %{(&bomb\|\|'^$\|utf-8'!~#&fileencoding?'\ '.&fileencoding.(&bomb?'-bom':''):'')
  \.('unix'!=#&fileformat?'\ '.&fileformat:'')}%)

" whitespace
set statusline+=%(\ \ %{&modifiable?(&expandtab?'et':'noet').'\ '.&shiftwidth:''}%)

set statusline+=\ %*\ %2v " Virtual column number.
set statusline+=\ %3p%%    " Percentage through file in lines as in |CTRL-G|


" colorscheme
"   solarized
hi StatusLine term=reverse cterm=reverse gui=reverse ctermfg=14 ctermbg=8 guifg=#93a1a1 guibg=#002b36
hi StatusLineNC term=reverse cterm=reverse gui=reverse ctermfg=11 ctermbg=0 guifg=#657b83 guibg=#073642
hi User1 ctermfg=14 ctermbg=0 guifg=#93a1a1 guibg=#073642
