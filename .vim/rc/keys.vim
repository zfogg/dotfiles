" rc/keys
scriptencoding utf-8


let g:mapleader=','

inoremap <C-c> <Esc>

noremap : <Nop>
noremap ; :
" and if you ever wanna map to ;
"noremap ;; ;

" write
nnoremap <Leader>w :w<CR>
nnoremap <Leader>W :sil! w<CR>
cnoremap w! w !sudo tee % >/dev/null

" quit
nnoremap <Leader>q :q<CR>

" vimrc
exe 'nn <silent> <Leader>v :'
    \.'cd '.g:dotvim_r             .' <Bar> '
    \.'$tabe! '                    .' <Bar> '
    \.'e '.g:myvimrc_l             .' <Bar> '
    \.'cd -'                       .' <Bar> '
    \.'exe "norm! zvzz"'           .' <Bar> '
    \.'<CR>'
exe 'nn <silent> <Leader>V :'
    \.'cd '.g:dotvim_r             .' <Bar> '
    \.'so '.g:myvimrc_l            .' <Bar> '
    \.'cd -'                       .' <Bar> '
    \.'filet detect'               .' <Bar> '
    \.'exe "norm! zvzz"'           .' <Bar> '
    \.'<CR>'

" Disabled default commands. {{{
    nnoremap Q       <Nop>
    noremap  <Up>    <Nop>
    noremap  <Down>  <Nop>
    noremap  <Left>  <Nop>
    noremap  <Right> <Nop>
    inoremap <Up>    <Nop>
    inoremap <Down>  <Nop>
    inoremap <Left>  <Nop>
    inoremap <Right> <Nop>
" }}}

" Yanking and pasting. {{{
    " Copy to system clipboard.
    noremap <Leader>y "+y
    " Paste from system clipboard.
    noremap <Leader>p "+p
    " Easily yank from cursor to EOL.
    nnoremap Y y$
    " Retain " register after pasting.
    xnoremap <expr> p v:register=='"'?'pgvy':'p'
" }}}

" Cursor motion. {{{
    call z#keys#Tmux()

    " j and k just work on long wrapped lines.
    nnoremap j gj
    nnoremap k gk

    " Scroll through items in the locations list.
    nmap <Leader>< <Plug>LocationPrevious
    nmap <Leader>> <Plug>LocationNext

    " A 'scrolling' effect.
    " FIXME: doesn't really work with in my iTerm+tmux setup.
    " nnoremap <a-j> <c-e>j
    " nnoremap <a-k> <c-y>k
    " nnoremap <a-l> zll
    " nnoremap <a-h> zhh

    " think of your left pinky
    inoremap jj    <Esc>
    inoremap kk    <Esc>:w<CR>

    " Leaving `Insert-mode` is too much sometimes.
    inoremap HH <C-o>^
    inoremap LL <C-o>$

    " Jump to matching pairs easily with tab.
    vnoremap <Tab> %
" }}}

" Folds. {{{
    nnoremap <Space> za
    vnoremap <Space> za
" }}}

" Tabs and splits. {{{
    nnoremap <Leader><C-t> :tabnew<CR>
    nnoremap <Leader>x     :tabclose<CR>
    nnoremap <Leader>j     :tabprevious<CR>
    nnoremap <Leader>k     :tabnext<CR>
    " Resize splits.
    nnoremap <S-Up>    5<C-W>+
    nnoremap <S-Down>  5<C-W>-
    nnoremap <S-Right> 5<C-W>>
    nnoremap <S-Left>  5<C-W><
    nnoremap   <Up>     <C-W>+
    nnoremap   <Down>   <C-W>-
    nnoremap   <Right>  <C-W>>
    nnoremap   <Left>   <C-W><
" }}}

" :make. {{{
    "nnoremap <Leader>M  :silent! make<CR>:redraw!<CR>
    "nnoremap <Leader>Mc :silent! make clean<CR>:redraw!<CR>
    "nnoremap <Leader>Mm :silent! make clean all<CR>:redraw!<CR>
    "nnoremap <Leader>Mt :cclose \| silent! make test           \| copen<CR>:AnsiEsc<CR>G:redraw!<CR>
    "nnoremap <Leader>MT :cclose \| silent! make clean all test \| copen<CR>:AnsiEsc<CR>G:redraw!<CR>
" }}}

" Et cetera. {{{
    " Previous matching command or search.
    cnoremap kk <Up>
    cnoremap HH <Home>
    cnoremap LL <Home>

    " Enter Replace mode from Visual mode.
    vnoremap R r<Space>R

    " Don't let `single-repeat` disturb the cursor's location.
    "map . .`[

    " Delete trailing whitespace.
    nnoremap <Leader>S :%s/\s\+$//<CR>:let @/=''<CR><c-o>

    " Fixes for display glitches.
    nnoremap <silent> <Leader><Space> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR>,<Space>
    nnoremap <Leader>rd :redraw!<CR>

    " Set the background to transparent.
    "nnoremap <Leader>bg :hi Normal ctermbg=NONE<CR>

    " Change to the directory of the current buffer's file.
    nnoremap <Leader>lcd :lcd %:p:h<BAR>pwd<CR>
    nnoremap <Leader>cd  :cd  %:p:h<BAR>pwd<CR>
" }}}
