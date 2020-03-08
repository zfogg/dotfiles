" rc/3.keys
scriptencoding utf-8


let mapleader = ','


inoremap <C-c> <Esc>

"noremap : <Nop>
noremap ; :
" and if you ever wanna map to ;
"noremap ;; ;

" Don't jump cursor when using * to search for word under cursor
noremap * *``
"noremap # #``

" Reselect visual block after indent/outdent
vnoremap < <gv
vnoremap > >gv

" Opposite of J. Split line at current point.
"nnoremap <M-j> i<CR><Esc>d^==kg_lD
noremap <A-S-j> a<CR><Esc>k$

" Keep cursor in centre of screen after motions
noremap n nzz
noremap <C-d> <C-d>zz
noremap <C-u> <C-u>zz
noremap { {zz
noremap } }zz
noremap gd gdzz

" write
nnoremap <Leader>w :w<CR>
"nnoremap <Leader>W :sil! w<CR>
cnoremap w! w !sudo tee % >/dev/null

" quit
nnoremap <Leader>q :q<CR>

" vimrc
if exists('g:myvimrc_f') && filereadable(g:myvimrc_f)
    exe 'nn <silent> <Leader>v :'
        \.'silent! $tabe! '.g:myvimrc_f        .' <Bar> '
        \.'silent! NERDTreeFromBookmark vimrc' .' <Bar> '
        \.'wincmd p'                           .' <Bar> '
        \.'<CR>'
    exe 'nn <silent> <Leader>V :'
        \.'so '.g:myvimrc_f                    .' <Bar> '
        \.'redraw!'                            .' <Bar> '
        \.'echom "⚠️  vimrc reloaded ‼️"'         .' <Bar> '
        \.'<CR>'
endif
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
    " Easily yank from cursor to EOL.
    noremap Y y$

    " Copy to system clipboard.
    noremap <Leader>y "+y
    " Copy short filename to system clipboard.
    nnoremap <Leader>y% :let @*=expand("%")<Bar>echo getreg('*')<CR>
    " Copy full filepath to system clipboard.
    nnoremap <Leader>Y% :let @*=expand("%:p")<Bar>echo getreg('*')<CR>

    " Paste from system clipboard.
    noremap <Leader>p "+p

    " Retain " register after pasting.
    xnoremap <expr> p v:register=='"'?'pgvy':'p'
" }}}


" Cursor motion. {{{
    call z#keys#Tmux()

    " j and k just work on long wrapped lines.
    noremap j gj
    noremap k gk

    " Scroll through items in the locations list.
    "nmap <Leader>< <Plug>LocationPrevious
    "nmap <Leader>> <Plug>LocationNext

    " A 'scrolling' effect.
    " FIXME: doesn't really work with in my iTerm+tmux setup.
    noremap <a-j> <c-e>j
    noremap <a-k> <c-y>k
    noremap <a-l> zll
    noremap <a-h> zhh


    " join lines: next to this
    inoremap JJ <C-o>J<C-o>==

    " join lines: this to prev
    inoremap KK <C-o>k<C-o>J<C-o>==

    " undo changes since InsertEnter
    inoremap uu <C-o>u

    "inoremap <C-r><C-r> <C-o><C-r>


    " think of your left pinky
    inoremap jj <Esc>
    inoremap kk <Esc>:w<CR>
    cnoremap kk <Up>
    cnoremap jj <Down>

    inoremap hH <Home>
    inoremap lL <End>
    cnoremap hH <Home>
    cnoremap lL <End>

    noremap  <PageUp>    <S-h>zz<S-l>
    inoremap <PageUp>    <C-o>zt
    nnoremap <PageDown>  <S-l>zz<S-h>
    inoremap <PageDown>  <C-o>zb

    noremap! <M-Right> <M-S-Right>
    inoremap <M-Right> <M-S-Right>
    inoremap <M-Left>  <M-S-Left>
    noremap! <M-Left>  <M-S-Left>

    " Jump to matching pairs easily with tab.
    "vnoremap <Tab> %
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
    " Enter Replace mode from Visual mode.
    vnoremap R r<Space>R

    " Don't let `single-repeat` disturb the cursor's location.
    "map . .`[

    " Delete trailing whitespace.
    nnoremap <Leader>S :call z#util#TrimWhitespace()<CR>

    " Fixes for display glitches.
    nnoremap <silent> <Leader><Space> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR>,<Space>
    nnoremap <Leader>rd :redraw!<CR>

    " Set the background to transparent.
    "nnoremap <Leader>bg :hi Normal ctermbg=NONE<CR>

    " Change to the directory of the current buffer's file.
    nnoremap <Leader>lcd :lcd %:p:h<BAR>pwd<CR>
    nnoremap <Leader>cd  :cd  %:p:h<BAR>pwd<CR>

    if exists('&inccommand')
        set inccommand=nosplit
    endif
" }}}


" files - search, sort, find, open {{{
" }}}
