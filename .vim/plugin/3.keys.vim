" rc/3.keys
scriptencoding utf-8


let mapleader      = ','
let maplocalleader = ','


inoremap <C-c> <Esc>

"noremap : <Nop>
noremap ; :
" and if you ever wanna map to ;
"noremap ;; ;

" Don't jump cursor when using * to search for word under cursor
noremap * *``
noremap # #``

" Reselect visual block after indent/outdent
vnoremap < <gv
vnoremap > >gv

" Opposite of J. Split line at current point.
"nnoremap <M-j> i<CR><Esc>d^==kg_lD
noremap <A-S-j> a<CR><Esc>k$

" Keep cursor in centre of screen after motions
"noremap n nzz
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
    let s:vimdir = resolve(fnamemodify(g:myvimrc, ':p:h'))
    "if PHas('nerdtree')
        exe 'nn <silent> <Leader>v :'
            \.'silent! $tabe! '.g:myvimrc_f                 .' <Bar> '
            \.'Fern '.s:vimdir.' -drawer -reveal=% -wait'   .' <Bar> '
            \.'wincmd p'                                    .' <Bar> '
            \.'<CR>'
    "endif
    exe 'nn <silent> <Leader>V :'
        \.'so '.g:myvimrc_f                    .' <Bar> '
        \.'redraw!'                            .' <Bar> '
        \.'echom "⚠️  vimrc reloaded ‼️"'        .' <Bar> '
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
    if exists('g:vscode')
        nmap <C-h> <Cmd>call VSCodeNotify('workbench.action.focusLeftGroup')<CR>
        nmap <C-j> <Cmd>call VSCodeNotify('workbench.action.focusBelowGroup')<CR>
        nmap <C-k> <Cmd>call VSCodeNotify('workbench.action.focusAboveGroup')<CR>
        nmap <C-l> <Cmd>call VSCodeNotify('workbench.action.focusRightGroup')<CR>
    "else
        "call z#keys#Tmux()
    endif

    " j and k just work on long wrapped lines.
    if exists('g:vscode')
    else
        noremap j gj
        noremap k gk
    endif

    " Scroll through items in the locations list.
    "INFO: can't map some keys :( use <Leader> I guess
    "nnoremap <C-S-{> :lnext<CR>
    "nnoremap <C-S-}> :lprevious<CR>
    nnoremap <Leader>, :lnext<CR>
    "nnoremap <Leader>< :lprevious<CR>
    nnoremap mm        :lprevious<CR>

    " A 'scrolling' effect.
    " FIXME: doesn't really work with in my iTerm+tmux setup.
    noremap <a-j> <c-e>j
    noremap <a-k> <c-y>k
    noremap <a-l> zll
    noremap <a-h> zhh


    " join lines: this to prev
    nnoremap <Leader>oO Dk$p<S-v>=
    inoremap <Leader>oO <C-o>D<C-o>k<C-o>$<C-o>p
    nnoremap <Leader>OO Dk$P<S-v>=
    inoremap <Leader>OO <C-o>D<C-o>k<C-o>$<C-o>P

    " join lines: next to this
    inoremap JJ <C-o>J<C-o>==

    " 1st 1st 1st
    " 2nd 2nd 2nd

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
    if exists('g:vscode')
        nnoremap <Leader><C-t> <Cmd>call VSCodeNotify('workbench.action.newWindowTab')<CR>
        nnoremap <Leader>x     <Cmd>call VSCodeNotify('keyboard-quickfix.openQuickFix')<CR>
        nnoremap <Leader>j     <Cmd>call VSCodeNotify('workbench.action.quickOpenPreviousRecentlyUsedEditorInGroup')<CR>
        nnoremap <Leader>k     <Cmd>call VSCodeNotify('workbench.action.quickOpenLeastRecentlyUsedEditorInGroup')<CR>
        " Resize splits.
        "nnoremap <S-Up>
        "nnoremap <S-Down>
        "nnoremap <S-Right>
        "nnoremap <S-Left>
        "nnoremap   <Up>
        "nnoremap   <Down>
        "nnoremap   <Right>
        "nnoremap   <Left>
    else
        nnoremap <Leader><C-t> :tabnew<CR>
        nnoremap <Leader>x     :tabclose<CR>
        nnoremap <Leader>J     :tabprevious<CR>
        nnoremap <Leader>K     :tabnext<CR>
        " Resize splits.
        nnoremap <S-Up>    5<C-W>+
        nnoremap <S-Down>  5<C-W>-
        nnoremap <S-Right> 5<C-W>>
        nnoremap <S-Left>  5<C-W><
        nnoremap   <Up>     <C-W>+
        nnoremap   <Down>   <C-W>-
        nnoremap   <Right>  <C-W>>
        nnoremap   <Left>   <C-W><
    endif
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
    "nnoremap <Leader>lcd :lcd %:p:h<BAR>pwd<CR>
    nnoremap <Leader>cd  :cd  %:p:h<BAR>pwd<CR>

    " INFO: https://github.com/markonm/traces.vim#neovim-v023
    if exists('&inccommand')
        set inccommand=nosplit
    endif
" }}}


" VSCode {{{
if exists('g:vscode')
    nnoremap z= <Cmd>call VSCodeNotify('keyboard-quickfix.openQuickFix')<CR>
endif
" }}}
