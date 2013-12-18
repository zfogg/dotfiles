" .vimrc | Vim 7.4 {{{1

    " Author: Zach Fogg - zach@zachfogg.com
    "  * @zfogg on GitHub and Twitter.
    "
    " Dependencies:
    "   * git
    "   * ctags, coffeetags, lushtags, fast-tags
    "   * ghc, ghci, hdevtools, ghc-mod
    "   * clang, eclim
    "   * hcoffeelint, jslint, lint

" }}}1


" Uninitialize Vim {{{1

filetype off " Unload plugins.

" Uninitialize Vim }}}1


" Key Mappings {{{1

    let mapleader=","

    " Opening, reading, writing, and closing. {{{2
        " Write from normal mode.
        map <Leader>w :w<CR>
        map <Leader>W :silent! w<CR>

        " Write with sudo.
        cmap w! w !sudo tee % >/dev/null

        " Open this configuration file.
        nmap <Leader>v :tabnew ~/.vimrc<CR>
        nmap <Leader>gv :tabnew ~/.gvimrc<CR>

        " Quit window.
        nnoremap <Leader>q :q<CR>
    " }}}2

    " Disabled default commands. {{{2
        nnoremap  Q  <nop>
        map     <up> <nop>
        map   <down> <nop>
        map   <left> <nop>
        map  <right> <nop>
        imap    <up> <nop>
        imap  <down> <nop>
        imap  <left> <nop>
        imap <right> <nop>
    " }}}2

    " Yanking and pasting. {{{2
        " Copy to system clipboard.
        map <Leader>y "+y

        " Paste from system clipboard.
        map <Leader>p "+p

        " Easily yank from cursor to EOL.
        nnoremap Y y$

        " Retain " register after pasting.
        xnoremap <expr> p v:register=='"'?'pgvy':'p'
    " }}}2

    " Cursor motion. {{{2
        " Splits
        nnoremap <C-J> <C-W>j
        nnoremap <C-K> <C-W>k
        nnoremap <C-L> <C-W>l
        nnoremap <C-H> <C-W>h

        " j and k just work on long wrapped lines.
        nnoremap j gj
        nnoremap k gk

        " Scroll through items in the locations list.
        nnoremap <up>   :lprev<CR>
        nnoremap <down> :lnext<CR>

        " A 'scrolling' effect.
        " FIXME: doesn't really work with in my term+tmux setup.
        nnoremap <a-j> <c-e>j
        nnoremap <a-k> <c-y>k
        nnoremap <a-l> zll
        nnoremap <a-h> zhh

        " It should be very easy to leave `Insert-mode`.
        inoremap jj <Esc><CR>
        inoremap kk <Esc>:w<CR>

        " Leaving `Insert-mode` is too much sometimes.
        inoremap HH <c-o>^
        inoremap LL <c-o>$

        " Jump to matching pairs easily with tab.
        vnoremap <Tab> %
    " }}}2

    " Folds. {{{2
        nnoremap <Space> za
        vnoremap <Space> za
    " }}}2

    " Tabs and splits. {{{2
        nnoremap <Leader>t :tabnew<CR>
        nnoremap <Leader>x :tabclose<CR>
        nnoremap <Leader>j :tabprevious<CR>
        nnoremap <Leader>k :tabnext<CR>

        " Resize splits.
        nnoremap <S-Up>    5<C-W>+
        nnoremap <S-Down>  5<C-W>-
        nnoremap <S-Right> 5<C-W>>
        nnoremap <S-Left>  5<C-W><
        nnoremap   <Up>     <C-W>+
        nnoremap   <Down>   <C-W>-
        nnoremap   <Right>  <C-W>>
        nnoremap   <Left>   <C-W><
    " }}}2

    " :make. {{{2
        nnoremap <Leader>M  :silent! make<CR>:redraw!<CR>
        nnoremap <Leader>Mc :silent! make clean<CR>:redraw!<CR>
        nnoremap <Leader>Mm :silent! make clean all<CR>:redraw!<CR>
        nnoremap <Leader>Mt :cclose \| silent! make test           \| copen<CR>:AnsiEsc<CR>G:redraw!<CR>
        nnoremap <Leader>MT :cclose \| silent! make clean all test \| copen<CR>:AnsiEsc<CR>G:redraw!<CR>
    " }}}

    " Plugin bindings. {{{2

        " NERDTree. {{{3
            nmap <Leader>n<Space> :NERDTreeToggle<CR>
        " }}}3

        " Fugitive. {{{3
            nmap <Leader>gc :Gcommit<CR>
            nmap <Leader>gd :Gdiff<CR>
            nmap <Leader>gl :Glog<CR>
            nmap <Leader>gs :Gstatus<CR>
            nmap <Leader>gR :Gread<CR>
            nmap <Leader>gW :Gwrite<CR>
            nmap <Leader>gp :Git push<CR>
        " }}}3

        " Java. {{{3
            nnoremap <Leader>J   :Java<CR>
            nnoremap <Leader>Jj  :silent! Java<CR>:redraw!<CR>
            nnoremap <Leader>Jc  :JavaCorrect<CR>
            nnoremap <Leader>Ji  :JavaImport<CR>
            nnoremap <Leader>JI  :JavaImpl<CR>
            nnoremap <Leader>Jgs :JavaGetSet<CR>
        " }}}3

        " Clojure. {{{3
            nnoremap <Leader>Cr :Require!<CR>
        "}}}3

        " Rainbow Parentheses. {{{3
            nnoremap <Leader>RPt :RainbowParenthesesToggleAll<CR>
        "}}}3

        " Slime. {{{3
            vnoremap <Leader>ss :SlimeSend<CR>
        "}}}3

        " CtrlP. {{{3
            nnoremap <C-F> :CtrlPLine<CR>
        " }}}3

        " Sessions. {{{3
            nmap <Leader>So :OpenSession<CR>
            nmap <Leader>Ss :SaveSession<CR>
            nmap <Leader>Sq :SaveSession<CR>:CloseSession<CR>
            nmap <Leader>SQ <Leader>Sq:q<CR>
        " }}}3

        " ghc-mod. {{{3
            nnoremap <Leader>Gc :GhcModCheckAndLintAsync<CR>
            nnoremap <Leader>Gt :GhcModType<CR>
            nnoremap <Leader>GT :GhcModTypeClear<CR>
            nnoremap <Leader>Ge :GhcModExpand<CR>
        " }}}3

        " hdevtools. {{{3
            nnoremap <Leader>Hi :HdevtoolsInfo<CR>
            nnoremap <Leader>Ht :HdevtoolsType<CR>
            nnoremap <Leader>Hc :HdevtoolsClear<CR>
        " }}}3

        " Tagbar. {{{3
            nnoremap <Leader>t<Space> :TagbarToggle<CR>
            let g:tagbar_type_markdown = {
                        \ 'ctagstype' : 'markdown',
                        \ 'kinds' : [
                        \   'h:Heading_L1',
                        \   'i:Heading_L2',
                        \   'k:Heading_L3'
                        \   ]
                        \ }
        " }}}3

        " gitgutter {{{2
        " }}}2
        
    " }}}2

    " Et cetera. {{{2
        " Reload vim settings.
        nnoremap <silent><Leader>V  :source ~/.vimrc<CR>:filetype detect<CR>:echo 'vimrc reloaded'<CR>
        nnoremap <silent><Leader>Gv :source ~/.gvimrc<CR>:filetype detect<CR>:echo 'gvimrc reloaded'<CR>

        " Previous matching command or search.
        cnoremap kk <Up>

        " Enter Replace mode from Visual mode.
        vnoremap R r<Space>R

        " Don't let `single-repeat` disturb the cursor's location.
        nnoremap . .`[

        " One less keypress to do use `Command-mode` from `Insert-mode`.
        imap <C-W> <C-O><C-W>

        " Delete trailing whitespace.
        nnoremap <Leader>S :%s/\s\+$//<cr>:let @/=''<CR><c-o>

        " Fixes for display glitches.
        nnoremap <Leader><Space> :nohlsearch<CR>
        nnoremap <Leader>rd      :redraw!<CR>
        " Set the background to transparent.
        nnoremap <Leader>bg :hi Normal ctermbg=NONE<CR>

        " Change to the directory of the current buffer's file.
        nnoremap <Leader>lcd :lcd %:p:h<CR>
        nnoremap <Leader>cd  :cd  %:p:h<CR>
    " }}}2

" Key Mappings }}}1


" Define Plugins {{{1

    set rtp+=~/.vim/bundle/vundle/
    call vundle#rc()

    Bundle 'gmarik/vundle'

    " Integrate with other programs. {{{2
        Bundle 'rking/ag.vim'
        Bundle 'tpope/vim-fireplace'
        Bundle 'tpope/vim-fugitive'
        Bundle 'airblade/vim-gitgutter'
        Bundle 'fs111/pydoc.vim'
        Bundle 'git://github.com/zfogg/vim-slime.git'
        Bundle 'Shougo/vimproc.vim'
    " }}}2

    " Add features and functionality. {{{2
        Bundle 'kien/ctrlp.vim'
        Bundle 'Lokaltog/vim-easymotion'
        Bundle 'scrooloose/nerdtree'
        Bundle 'xolox/vim-session'
        Bundle 'ervandew/supertab'
        Bundle 'scrooloose/syntastic'
        Bundle 'SirVer/ultisnips'
        Bundle 'Valloric/YouCompleteMe'
    " }}}2

    " Tags. {{{2
        Bundle 'Tagbar'
        Bundle 'lukaszkorecki/CoffeeTags'
        Bundle 'xolox/vim-easytags'
        Bundle 'bitc/lushtags'
    " }}}2

    " Plugin plugins. {{{2
        Bundle 'suy/vim-ctrlp-commandline'
        Bundle 'kana/vim-textobj-user'
    " }}}2

    " Language support. {{{2
        Bundle 'guns/vim-clojure-static'
        Bundle 'kchmck/vim-coffee-script'
        Bundle 'ap/vim-css-color'
        Bundle 'mattn/emmet-vim'
        Bundle 'eagletmt/ghcmod-vim'
        Bundle 'lukerandall/haskellmode-vim'
        Bundle 'bitc/vim-hdevtools'
        Bundle 'digitaltoad/vim-jade'
        Bundle 'pangloss/vim-javascript'
        Bundle 'jelera/vim-javascript-syntax'
        Bundle 'groenewege/vim-less'
        Bundle 'plasticboy/vim-markdown'
        Bundle 'juvenn/mustache'
        Bundle 'wavded/vim-stylus'
        "Bundle 'marijnh/tern_for_vim'
    " }}}2

    " Colorschemes. {{{2
        Bundle 'chriskempson/base16-vim'
        Bundle 'git://github.com/zfogg/vim-eddie.git'
        Bundle 'Pychimp/vim-luna'
        Bundle 'google_prettify.vim'
        Bundle 'junegunn/seoul256.vim'
        Bundle 'chriskempson/vim-tomorrow-theme'
        Bundle 'altercation/vim-colors-solarized'
    " }}}2

    " Beautify Vim. {{{2
        Bundle 'nathanaelkane/vim-indent-guides'
        Bundle 'itchyny/lightline.vim'
        Bundle 'kien/rainbow_parentheses.vim'
        Bundle 'AnsiEsc.vim'
    " }}}2

    " Direct text manipulation. {{{2
        Bundle 'b4winckler/vim-angry'
        Bundle 'junegunn/vim-easy-align'
        Bundle 'tommcdo/vim-exchange'
        Bundle 'Raimondi/delimitMate'
        Bundle 'tsaleh/vim-matchit'
        Bundle 'scrooloose/nerdcommenter'
        Bundle 'tpope/vim-surround'
        Bundle 'lucapette/vim-textobj-underscore'
        Bundle 'kana/vim-textobj-indent'
        Bundle 'tpope/vim-unimpaired'
    " }}}2

    " Silent enhancements. {{{2
        Bundle 'xolox/vim-misc'
        Bundle 'kana/vim-niceblock'
        Bundle 'sickill/vim-pasta'
        Bundle 'tpope/vim-repeat'
        Bundle 'tpope/vim-speeddating'
    " }}}2

" Plugins }}}1


" Basic Settings {{{1

    set nocompatible              " Activate Vim only features and mappings.
    set number                    " Display line numbers.
    set numberwidth=1             " Numberglyphs use only one column.
    set title                     " Show file title in console title bar.
    set wildmenu                  " Menu completion in command mode on <Tab>.
    set wildmode=full             " <Tab> cycles between all matching choices.
    set showtabline=1             " Show buffer tabs when more than one.
    set pastetoggle=<F1>          " For 'Insert' mode pasting with <S-Insert>."

    " Pretty colors.
    syntax on                     " Smart syntax highlighing.
    set t_Co=256                  " To enable 256bit colors in the console.

    " Ignore these files when completing.
    set wildignore+=*.o,*.obj,.git,*.pyc,*.class,*~

    " Don't sound the system bell or blink the screen on error.
    set noerrorbells visualbell t_vb=
    autocmd GUIEnter * set visualbell t_vb=

    " Save change history so you can <Undo> after you closed vim or reboot.
    if has('persistent_undo')
        set undofile undodir=$HOME/.vim/.undo,.,/var/tmp,/tmp
    endif

    " Don't litter swapfiles all over.
    set directory=$HOME/.vim/.swap//,.//,/var/tmp//,/tmp//
    set history=1024

    " Moving around and editing." {{{2
        set nostartofline           " Avoid moving cursor to BOL when jumping around.
        set virtualedit=all         " Let cursor move past $ in command mode.
        set backspace=2             " Allow backspacing over autoindent, EOL, and BOL.
        set autoindent              " Always set autoindenting on.
        set lazyredraw              " For better macro performance.
    "}}}2

    " Tiny aesthetic tweaks." {{{2
        set cursorline              " A horizontal line for the cursor location.
        set ruler                   " Show the cursor position all the time.
        set scrolloff=3             " Keep n context lines above and below the cursor.
        set showmatch               " Briefly jump to a paren once it's balanced.
        set list                    " Visually display tabs and trailing whitespace.
        set listchars=tab:>-,trail:-,precedes:<,extends:>
    "}}}2

    " Whitespace." {{{2
        set shiftwidth=4            " An indent level is n spaces.
        set expandtab               " Use spaces, not tabs, for autoindent/tab key.
        set tabstop=4               " <Tab> inserts n spaces.
        set softtabstop=4           " <BS> over an autoindent deletes both spaces.
        set shiftround              " Rounds indent to a multiple of shiftwidth.
        set linebreak               " Don't wrap textin the middle of a word.
        set nowrap                  " Don't wrap text.
        set textwidth=0             " Don't break lines.
        set wrapmargin=0            " Seriously, don't break lines.
    "}}}2

    " Folding. {{{2
        set foldenable
        set foldmethod=syntax
        set foldopen=mark,percent,quickfix,tag,undo
    "}}}2

    " Reading and writing. {{{2
        set noautowrite             " Never write a file unless I request it.
        set noautowriteall          " NEVER.
        set noautoread              " Don't automatically re-read changed files.
        set modeline                " Allow vim options to be embedded in files;
        set modelines=5             " they must be within the first or last 5 lines.
        set ffs=unix,dos,mac        " Try recognizing dos, unix, and mac line endings.
    "}}}2

    " Messages, info, and statuses. {{{2
        set ls=2                    " allways show status line
        set confirm                 " Y-N-C prompt if closing with unsaved changes.
        set showcmd                 " Show incomplete normal mode commands as I type.
        set report=0                " : commands always print changed line count.
        set shortmess+=a            " Use [+]/[RO]/[w] for modified/readonly/written.
        set ruler                   " Show some info, even without statuslines.
        set laststatus=2            " Always show statusline, even if only 1 window.
        set statusline=[%l,%v\ %P%M]\ %f\ %r%h%w\ (%{&ff})\ %{fugitive#statusline()}
    "}}}2

    " Searching and Patterns {{{2
        set ignorecase              " Default to using case insensitive searches.
        set smartcase               " unless uppercase letters are used in the regex.
        set smarttab                " Handle tabs more intelligently.
        set hlsearch                " Highlight searches by default.
        set incsearch               " Incrementally search while typing a /regex.
    "}}}2

    if executable("ag")
        set grepprg=ag\ --nogroup\ --nocolor
    else
        set grepprg=grep\ --exclude-dir\ .git\ -nrI\ $*\ .\ /dev/null
    endif

    " Sessions
    " The 'Session' plugin does this on save.
    set sessionoptions-=options

    " Per-project .vimrc.
    set exrc
    set secure

" Basic Settings }}}1


" Plugin Settings {{{1

    " Eclim {{{2
        let g:EclimCompletionMethod = 'omnifunc'
    " }}}2

    " NERDTree {{{2
        let NERDTreeIgnore = [
            \ '.class$',
            \ '.pyc$',
            \ '^node_modules$',
            \ '.o$'
        \ ]
    " }}}2

    " Slime {{{2
        "let g:slime_config    = {"sessionname": "repl", "windowname": "1"}
        let g:slime_target     = "tmux"
        let g:slime_paste_file = tempname()
    " }}}2

    " Indent-Guides {{{2
        let g:indent_guides_start_level           = 2
        let g:indent_guides_guide_size            = 1
        let g:indent_guides_enable_on_vim_startup = 1
        let g:indent_guides_auto_colors           = 0
        augroup IndentGuides
            autocmd!
            autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  ctermbg=grey
            autocmd VimEnter,Colorscheme * :hi IndentGuidesEven ctermbg=darkgrey
        augroup END
    " }}}2

    " Rainbow Parentheses {{{2
        let g:rbpt_colorpairs = [
            \ ['brown',       'RoyalBlue3'],
            \ ['blue',        'SeaGreen3'],
            \ ['darkgray',    'DarkOrchid3'],
            \ ['darkgreen',   'firebrick3'],
            \ ['darkmagenta', 'DarkOrchid3'],
            \ ['darkcyan',    'RoyalBlue3'],
            \ ['darkred',     'SeaGreen3'],
            \ ['brown',       'firebrick3'],
            \ ['gray',        'RoyalBlue3'],
            \ ['darkmagenta', 'DarkOrchid3'],
            \ ['blue',        'firebrick3'],
            \ ['red',         'firebrick3'],
            \ ['darkgreen',   'RoyalBlue3'],
            \ ['darkcyan',    'SeaGreen3'],
            \ ['darkred',     'DarkOrchid3'],
        \ ]

        let g:rbpt_nax            = 16
        let g:rbpt_loadcmd_toggle = 0

        au VimEnter * silent! RainbowParenthesesToggle
        au Syntax * silent! RainbowParenthesesLoadRound
        au Syntax * silent! RainbowParenthesesLoadSquare
        au Syntax * silent! RainbowParenthesesLoadBraces
    " }}}2

    " YouCompleteMe {{{2
        let g:ycm_autoclose_preview_window_after_completion = 1
        let g:ycm_autoclose_preview_window_after_insertion  = 1
        let g:ycm_register_as_syntastic_checker             = 1
        let g:ycm_complete_in_comments                      = 1

        let g:ycm_seed_identifiers_with_syntax              = 1
        let g:ycm_collect_identifiers_from_tags_files       = 1

        let g:ycm_extra_conf_globlist                       = ['~/code/*']

        let g:ycm_semantic_triggers =  {
            \ 'c,objc'                  : ['->', '.'],
            \ 'cpp,objcpp'              : ['->', '.', '::'],
            \ 'haskell'                 : ['.'],
            \ 'java'                    : ['.'],
            \ 'javascript,coffee'       : ['.'],
            \ 'ocaml'                   : ['.',  '#'],
            \ 'php'                     : ['-,', '::'],
            \ 'python'                  : ['.'],
            \ 'vim'                     : ['.'],
        \ }

        let g:ycm_filetype_blacklist = {
            \ 'tagbar':   1,
            \ 'qf':       1,
            \ 'notes':    1,
            \ 'markdown': 1,
            \ 'unite':    1,
            \ 'text':     1,
            \ 'vimwiki':  1,
        \}
    " }}}2

    " Syntastic {{{2
        let g:syntastic_always_populate_loc_list = 1
    " }}}2

    " UltiSnips {{{2
        let g:UltiSnipsDontReverseSearchPath="1"
        function! g:UltiSnips_Complete()
            call UltiSnips_ExpandSnippet()
            if g:ulti_expand_res == 0
                if pumvisible()
                    return "\<C-n>"
                else
                    call UltiSnips_JumpForwards()
                    if g:ulti_jump_forwards_res == 0
                    return "\<TAB>"
                    endif
                endif
            endif
            return ""
        endfunction

        au BufEnter * exec "inoremap <silent> " . g:UltiSnipsExpandTrigger . " <C-R>=g:UltiSnips_Complete()<cr>"
        let g:UltiSnipsJumpForwardTrigger="<tab>"
        let g:UltiSnipsExpandTrigger="<c-e>"
    " }}}2

    " CtrlP {{{2
        let g:ctrlp_map               = '<c-p>'
        let g:ctrlp_cmd               = 'CtrlP'
        let g:ctrlp_user_command      = 'ag %s -l --nocolor --hidden -g ""'
        let g:ctrlp_working_path_mode = 'ra'
        let g:ctrlp_custom_ignore     = 'node_modules\|bower_components\|git'
        let g:ctrlp_extensions        = ['mixed', 'tag', 'line', 'dir', 'commandline']
        set wildignore+=*/tmp/*,*.so,*.swp,*.zip
    " }}}2

    " EasyMotion {{{2
        "let g:EasyMotion_leader_key = '<Leader>'
    " }}}2

    " Lightline {{{2
        let g:lightline = {
            \ 'colorscheme': 'jellybeans',
            \ 'active': {
            \   'left': [ [ 'mode', 'paste' ], [ 'fugitive', 'filename' ], ['ctrlpmark'] ],
            \   'right': [ [ 'syntastic', 'lineinfo' ], ['percent'], [ 'fileformat', 'fileencoding', 'filetype' ] ]
            \ },
            \ 'component_function': {
            \   'fugitive': 'MyFugitive',
            \   'filename': 'MyFilename',
            \   'fileformat': 'MyFileformat',
            \   'filetype': 'MyFiletype',
            \   'fileencoding': 'MyFileencoding',
            \   'mode': 'MyMode',
            \   'ctrlpmark': 'CtrlPMark',
            \ },
            \ 'component_expand': {
            \   'syntastic': 'SyntasticStatuslineFlag',
            \ },
            \ 'component_type': {
            \   'syntastic': 'error',
            \ },
            \ 'component': {
            \   'readonly': '%{&readonly?"":""}',
            \ },
            \ 'separator': { 'left': '', 'right': '' },
            \ 'subseparator': { 'left': '', 'right': '' }
        \ }

        function! MyModified()
            return &ft =~ 'help' ? '' : &modified ? '+' : &modifiable ? '' : '-'
        endfunction

        function! MyReadonly()
            return &ft !~? 'help' && &readonly ? 'RO' : ''
        endfunction

        function! MyFilename()
            let fname = expand('%:t')
            return fname == 'ControlP' ? g:lightline.ctrlp_item :
                    \ fname == '__Tagbar__' ? '' :
                    \ fname =~ '__Gundo\|NERD_tree' ? '' :
                    \ &ft == 'vimfiler' ? vimfiler#get_status_string() :
                    \ &ft == 'unite' ? unite#get_status_string() :
                    \ &ft == 'vimshell' ? vimshell#get_status_string() :
                    \ ('' != MyReadonly() ? MyReadonly() . ' ' : '') .
                    \ ('' != fname ? fname : '[No Name]') .
                    \ ('' != MyModified() ? ' ' . MyModified() : '')
        endfunction

        function! MyFugitive()
            try
                if expand('%:t') !~? 'Tagbar\|Gundo\|NERD' && &ft !~? 'vimfiler' && exists('*fugitive#head')
                    let mark = ''  " edit here for cool mark
                    let _ = fugitive#head()
                    return strlen(_) ? mark._ : ''
                endif
            catch
            endtry
            return ''
        endfunction

        function! MyFileformat()
            return winwidth(0) > 70 ? &fileformat : ''
        endfunction

        function! MyFiletype()
            return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype : 'no ft') : ''
        endfunction

        function! MyFileencoding()
            return winwidth(0) > 70 ? (strlen(&fenc) ? &fenc : &enc) : ''
        endfunction

        function! MyMode()
            let fname = expand('%:t')
            return fname == '__Tagbar__' ? 'Tagbar' :
                    \ fname == 'ControlP' ? 'CtrlP' :
                    \ fname == '__Gundo__' ? 'Gundo' :
                    \ fname == '__Gundo_Preview__' ? 'Gundo Preview' :
                    \ fname =~ 'NERD_tree' ? 'NERDTree' :
                    \ &ft == 'unite' ? 'Unite' :
                    \ &ft == 'vimfiler' ? 'VimFiler' :
                    \ &ft == 'vimshell' ? 'VimShell' :
                    \ winwidth(0) > 60 ? lightline#mode() : ''
        endfunction

        function! CtrlPMark()
            if expand('%:t') =~ 'ControlP'
                call lightline#link('iR'[g:lightline.ctrlp_regex])
                return lightline#concatenate([g:lightline.ctrlp_prev, g:lightline.ctrlp_item
                        \ , g:lightline.ctrlp_next], 0)
            else
                return ''
            endif
        endfunction

        let g:ctrlp_status_func = {
            \ 'main': 'CtrlPStatusFunc_1',
            \ 'prog': 'CtrlPStatusFunc_2',
        \ }

        function! CtrlPStatusFunc_1(focus, byfname, regex, prev, item, next, marked)
            let g:lightline.ctrlp_regex = a:regex
            let g:lightline.ctrlp_prev = a:prev
            let g:lightline.ctrlp_item = a:item
            let g:lightline.ctrlp_next = a:next
            return lightline#statusline(0)
        endfunction

        function! CtrlPStatusFunc_2(str)
            return lightline#statusline(0)
        endfunction

        let g:tagbar_status_func = 'TagbarStatusFunc'

        function! TagbarStatusFunc(current, sort, fname, ...) abort
            let g:lightline.fname = a:fname
            return lightline#statusline(0)
        endfunction

        augroup AutoSyntastic
            autocmd!
            autocmd BufWritePost *.c,*.cpp call s:syntastic()
        augroup END
        function! s:syntastic()
            SyntasticCheck
            call lightline#update()
        endfunction
    " }}}2

    " Session {{{2
        let g:session_default_name      = 'default'           " When you don't name your session.
        let g:session_command_aliases   = 1                   " Use commands prefixed with 'Session'.
        let g:session_autosave          = 'yes'               " Save on exit sans prompt.
        let g:session_autosave_periodic = 1                   " Save every 'n' minutes.
        let g:session_autoload          = 0                   " Don't ask to open default session.
        let g:session_verbose_messages  = 0                   " Disable noisy save messages.

        " Persist the options of the session plug-in using the session plug-in...
        let g:session_persist_globals = ['&sessionoptions'] " Specific or custom variables or options.
        call add(g:session_persist_globals, 'g:session_autoload')
        call add(g:session_persist_globals, 'g:session_autosave')
        call add(g:session_persist_globals, 'g:session_default_to_last')
        call add(g:session_persist_globals, 'g:session_persist_globals')
    " }}}2

    " delimitMate {{{2
        " Intuitive indentation and delimiter expansion.
        let delimitMate_expand_cr = 1
        imap <expr><CR> pumvisible() ? "\<C-n>" : "<Plug>delimitMateCR"
    " }}}2

    " haskellmode {{{2
        let g:haddock_browser = '/usr/bin/chromium'
        let g:haddock_docdir = '/usr/share/doc/ghc/html/'
        autocmd VimEnter * set cmdheight=1
    " }}}2

    " easy-align {{{2
        vnoremap <silent> <Enter> :EasyAlign<Enter>
    " }}}2

    " coffeetags {{{2
        if executable('coffeetags')
            let g:tagbar_type_coffee = {
                \ 'ctagsbin' : 'coffeetags',
                \ 'ctagsargs' : '',
                \ 'kinds' : [
                    \ 'f:functions',
                    \ 'o:object',
                \ ],
                \ 'sro' : ".",
                \ 'kind2scope' : {
                    \ 'f' : 'object',
                    \ 'o' : 'object',
                \ }
            \ }
        endif
    " }}}2

    " easytags {{{2
        let g:easytags_file          = '~/.vim/tags'
        let g:easytags_dynamic_files = 1
        let g:easytags_events        = ['BufReadPost', 'BufWritePost']
    " }}}2

    " gitgutter {{{2
        let g:gitgutter_diff_args = '-w'
        let g:gitgutter_realtime  = 0
        let g:gitgutter_eager     = 0
    " }}}2

" Plugin Settings }}}1


" FileType Settings {{{1

    augroup AllFiles
        au!
        " Return cursor to last position.
        au BufReadPost *
            \ if ! exists("g:leave_my_cursor_position_alone")   |
            \   if line("'\"") > 0 && line ("'\"") <= line("$") |
            \     exe "normal g'\""                             |
            \   endif                                           |
            \ endif
        " git-inline-diff on read,write
        au BufReadPost,BufWritePost * exe "normal " . mapleader . "d"
    augroup END

    augroup VimL
        au!
        au FileType vim setl ts=4 sts=4 sw=4
        au FileType vim setl fdm=marker
        au FileType vim setl omnifunc=
    augroup END

    augroup C_Cpp
        au!
        au FileType c,c++ setl sw=4 ts=4 sts=4
        au FileType c,c++ setl fdm=marker fmr={,}
    augroup END

    augroup JavaScript
        au!
        au FileType javascript setl sw=4 ts=4 sts=4
        au FileType javascript setl fdm=marker fmr={,}
    augroup END

    augroup Java
        au!
        au FileType java setl sw=4 ts=4 sts=4 fdm=marker fmr={,} fdl=1
    augroup END

    augroup CoffeeScript
        au!
        au FileType coffee setl expandtab sw=2 ts=2 sts=2 fdm=indent nofoldenable
        au BufWritePost *.coffee CoffeeLint! | cwindow | redraw!
    augroup END

    augroup HTML
        au!
        au FileType html,xhtml,xml,css setl expandtab sw=4 ts=4 sts=4
        " Templating languages.
        au FileType mustache,handlebars setl expandtab sw=4 ts=4 sts=4
    augroup END

    augroup Python
        au!
        au FileType python setl expandtab sw=4 sts=4 cinwords=if,elif,else,for,while,try,except,finally,def,class,with
        au FileType python set omnifunc=pythoncomplete#Complete
        au FileType python set efm=%C\ %.%#,%A\ \ File\ \"%f\"\\,\ line\ %l%.%#,%Z%[%^\ ]%\\@=%m
    augroup END

    augroup Clojure
        au!
        let g:clojure_foldwords = "def,ns"
        au FileType clojure setl sw=2
        au FileType clojure setl foldmethod=indent foldminlines=2 foldnestmax=2
    augroup END

    augroup Haskell
        au!
        au FileType haskell setl sw=4 ts=4 sts=4 si
        au BufRead,BufNewFile *.hs compiler ghc
        au BufWritePost *.hs,*.hsc silent !fast-tags %
        au BufWritePost *.hs,*.hsc SlimeSend1 :r
    augroup END

    augroup Jade
        au!
        au FileType jade setl sw=2 ts=2 sts=2 si
    augroup END

    augroup Markdown
        au!
        au BufRead,BufNewFile *.md setl ft=markdown
    augroup END

    augroup Stylus
        au!
        au BufRead,BufNewFile *.styl setl sw=2 ts=2 sts=2 si
    augroup END

" Filetype Settings }}}1


" Initialize Vim {{{1

    filetype plugin indent on           " Load plugins.

    try                                 " Fall back to preferred colorschemes.
        colorscheme evil-eddie
    catch /^Vim\%((\a\+)\)\=:E185/
        try
            colorscheme jellybeans
        catch /^Vim\%((\a\+)\)\=:E185/
            colorscheme desert256
        endtry
    endtry

" Initialize Vim }}}1

