" rc/settings
"  vim: foldmethod=marker:
scriptencoding utf-8


"set nonumber numberwidth=2  " hybrid line numbers - relative & static
set number numberwidth=2     " hybrid line numbers - relative & static
set title                   " file title in the $TERM titlebar
set showtabline=1           " show buffer tabs when more than `n`
set pastetoggle=<F1>        " for 'Insert' mode pasting with <S-Insert>
set history=2048            " persist mode-cmdline history
set noerrorbells visualbell " no system bell, no screen blink on error
if exists('$SHELL')
    set shell=$SHELL
endif

set updatetime=220         " for CursorHold autocmd (milliseconds)
let g:netrw_dirhistmax=0   " http://www.vim.org/scripts/script.php?script_id=1075

" FIXME: danger lol
set exrc
set secure

" for :mksession
set sessionoptions-=blank,buffers,localoptions,help
set sessionoptions+=curdir,globals,options,tabpages
set sessionoptions+=folds,resize,winpos,winsize
" for :mksession also
set viewoptions-=options,localoptions
set viewoptions+=cursor,curdir,folds


" {{{ 'path'  'cdpath'  file-searching
    set path=.,,**

    let &cdpath = ','.substitute(substitute($CDPATH, '[, ]', '\\\0', 'g'), ':', ',', 'g')
    set cdpath+=.,,**
" }}} 'path'  'cdpath'  file-searching


" {{{ wild, undo, swap, backup
    set wildmenu wildignorecase
    set wildchar=<Tab>
    "set wildmode=list:longest,full
    set wildmode=longest:full,full
    " NOTE: 'wildignore'  (default: v:null)
    set wildignore=
    set wildignore+=*.o,*.obj,*.so,*.exe,*.dll,*.manifest,*.dmg
    set wildignore+=*.swp,*.pyc,*.class
    set wildignore+=*.tar,*.bz,*.gz,*.xz,*.zip,*.7z,*.rar
    set wildignore+=*/.git/*,*/node_modules/*
    set wildignore+=*.swc,*.swc.old
    set wildignore+=*.DS_Store
    set wildignore+=*~,~*

    if has('nvim')
        set wildoptions=pum
        "set pumheight=7
        set pumheight=25
        set pumwidth=48
        set pumblend=32
    endif

    "set complete-=i
    set complete+=i
    set complete-=t
    "set completeopt=menuone,preview,noinsert,noselect
    "set completeopt=menu,menuone,preview,noinsert,noselect
    "set completeopt=menuone,preview,noinsert,noselect
    set completeopt=menu,menuone,preview
    set conceallevel=2 concealcursor=nvic

    let s:unix_dictionary='/usr/share/dict/words'
    if has('unix') && filereadable(s:unix_dictionary)
        let &dictionary=s:unix_dictionary
    endif

    if has('persistent_undo')
        set undofile
    endif

    set swapfile
    set backupdir-=.
    set directory-=.
    set backupdir-=.
    "set   undodir+=$TMPDIR.'/'
    "set directory+=$TMPDIR.'/'
    "set backupdir+=$TMPDIR.'/'

    let g:omni_sql_no_default_maps = 1

    if has('wildignore')
        let s:backupskip_dirs = []
        for s:dir_name in split(&undodir, ',') + split(&directory, ',') + split(&backupdir, ',')
            call add(s:backupskip_dirs, fnamemodify(s:dir_name, ':p:h').'/*')
        endfor
        let &backupskip.=join(s:backupskip_dirs, ',')
    endif
" }}} undo / redo, swap, backup


" {{{ Moving around and editing
    set nostartofline              " Avoid moving cursor to BOL when jumping around.
    set virtualedit=all            " Let cursor move past $ in command mode.
    set backspace=indent,eol,start " Allow backspacing over autoindent, EOL, and BOL.
    set autoindent                 " Always set autoindenting on.
    "set lazyredraw                 " For better macro performance.
    set redrawtime=1200            " For better macro performance.
    set maxmempattern=100000       " For better macro performance.
    set ttimeoutlen=100            " Time (ms) for a key code sequence to complete.
    "augroup RcSettings_timeoutlen
        "au!
        "" The time (ms) for a mapped sequence to complete.
        "autocmd InsertEnter * set timeoutlen=400
        "autocmd InsertLeave * set timeoutlen=900
    "augroup END
" }}} Moving around and editing


" {{{ 'cpoptions'
    "   default:aABceFs
    "   mine:   aABcdEefFIMnoPqRstWXZ+;_
    set cpo+=aA        " cpo-a cpo-A
    set cpo-=b cpo+=B  " cpo-b cpo-B
    set cpo+=c cpo-=C  " cpo-c cpo-C
    set cpo+=d cpo-=D  " cpo-d cpo-D
    set cpo+=Ee        " cpo-E cpo-e
    set cpo+=fF        " cpo-f cpo-F
    "set cpo+=I         " cpo-I
    set cpo-=i         " cpo-i
    set cpo-=J         " cpo-J
    set cpo-=K         " cpo-K
    set cpo-=lL        " cpo-l cpo-L
    set cpo-=m cpo+=M  " cpo-m cpo-M
    set cpo+=n         " cpo-n
    set cpo+=o cpo-=O  " cpo-o cpo-O
    set cpo-=p cpo+=P  " cpo-p cpo-P
    set cpo+=q         " cpo-q
    set cpo-=r cpo+=R  " cpo-r cpo-R
    set cpo+=s cpo-=S  " cpo-s cpo-S
    set cpo+=t         " cpo-t
    set cpo-=u         " cpo-u
    set cpo-=v         " cpo-v
    set cpo+=W         " cpo-W
    set cpo-=x cpo+=X  " cpo-x cpo-X
    set cpo-=y         " cpo-y
    set cpo+=Z         " cpo-Z
    set cpo-=! cpo-=$  " cpo-! cpo-$
    set cpo-=% cpo+=+  " cpo-% cpo-+
    set cpo-=> cpo+=;  " cpo-> cpo-;
    if has('nvim')
        set cpo+=_     " cpo-_
    endif
" }}} 'cpoptions'


" {{{ 'formatoptions'
    "   default: "tcqj"
    "   mine: "cjlnqrwo"
    set formatoptions+=c  " Autowrap comments using textwidth - :help fo-table
    set formatoptions+=j  " Delete comment character when joining commented lines
    set formatoptions+=l  " do not wrap lines that have been longer when starting insert mode already
    set formatoptions+=n  " Recognize numbered lists
    set formatoptions+=q  " Allow formatting of comments with "gq"
    set formatoptions+=r  " Insert comment leader after hitting <Enter>
    set formatoptions+=w  " A whitespace line ending indicates a continued paragraph.
    set formatoptions-=t  " Don't wrap text using textwidth
    set formatoptions+=o  " Insert the current comment leader 'o' or 'O' in Normal mode.
" }}} 'formatoptions'


" {{{ Tiny aesthetic tweaks
    set cul nocuc       " A horizontal line for the cursor location.
    set ruler           " Show the cursor position all the time.
    set scrolljump=4    " Scroll n lines at a time at bottom/top
    set scrolloff=4     " Keep n context lines above and below the cursor.
    set sidescroll=1    " FIXME
    set sidescrolloff=8 " FIXME
    set showmatch       " Briefly jump to a paren once it's balanced.
    set list            " Visually display tabs and trailing whitespace.
    set listchars=
        \eol:¬,
        \tab:→\ ,
        \trail:·,
        \extends:⟩,
        \precedes:⟨,
        \nbsp:␣
    set fillchars=
        \vert:\ ,
        \stl:\ ,
        \stlnc:\ 
" }}} Tiny aesthetic tweaks


" {{{ Whitespace
    set expandtab       " Use spaces, not tabs, for autoindent/tab key.
    set copyindent
    set preserveindent
    set tabstop=2       " <Tab> inserts n spaces.
    set softtabstop=2   " <BS> over an autoindent deletes both spaces.
    set shiftwidth=0    " An indent level is n spaces.
    set shiftround      " Rounds indent to a multiple of shiftwidth.
    set nowrap          " Don't wrap text.
    set linebreak       " Don't wrap textin the middle of a word.
    set wrapmargin=0    " Seriously, don't break lines.
    set textwidth=0     " Don't break lines.
    set showbreak=↪\    " Visualize wrapped lines.
" }}} Whitespace


" {{{ Folding
    set foldenable
    set foldmethod=syntax
    set foldopen+=percent,quickfix,tag,undo
    set foldnestmax=3
    set foldminlines=3

    "let g:vimsyn_folding='afp'
    let g:fastfold_skip_filetypes = [
        \ 'taglist'
        \,'fern'
        \,'nerdtree'
        \,'help'
        \,'vim'
    \]
" }}} Folding


" Reading and writing. {{{
    set noautowrite      " Never write a file unless I request it.
    set noautowriteall   " NEVER.
    set noautoread       " Don't automatically re-read changed files.
    set modeline         " Allow vim options to be embedded in files.
    set modelines=3      " look in the first or last `n` lines.
    set ffs=unix,mac,dos " Try recognizing dos, unix, and mac line endings.
" }}}


" shortmess {{{
    " NOTE: Vim default "filnxtToOF"
    "set shortmess=aOstTI " :help 'shortmess'
    set shm+=a  " shortcut for shm+=ailmnrwx
    set shm-=O
    set shm+=O
    set shm+=s
    set shm+=t
    set shm+=T
    set shm-=W
    set shm-=A
    set shm+=I
    set shm+=c
    set shm-=q
    set shm-=F
" }}}


" Messages, info, and statuses. {{{
    set confirm          " Y-N-C prompt if closing with unsaved changes.
    set showcmd          " Show incomplete normal mode commands as I type.
    set noshowmode       " Don't show the current editor mode in the cmd line.
    set report=0         " : commands always print changed line count.
    set ruler            " Show some info, even without statuslines.
    set laststatus=2     " Always show statusline, even if only 1 window.
" }}}


" {{{ Searching and Patterns
    set ignorecase      " Default to using case insensitive searches.
    set smartcase       " unless uppercase letters are used in the regex.
    set smarttab        " Handle tabs more intelligently.
    set smartindent     " Indent intelligently.
    set cindent         " Indent even more intelligently.
    set hlsearch        " Highlight searches by default.
    set incsearch       " Incrementally search while typing a /regex.
    set regexpengine=1  " Auto-switch regexp engines if workload hangs.

    " http://www.ascii-code.com/
    "         '@'       == /[a-zA-Z]/ == filter(isalpha, $ascii_arr)
    "         ,48-57,   == /[0-9]/    == filter(isdigit, $ascii_arr)
    "         ,192-255, == /[À-ÿ]/    /* accented characters - extended ascii */
    "         ,,,       == /,/
    "         ,@-@,     == /@/
    "         ,^,,      == /[^,]/
    "         ,^<EOL>   == /\^/
    "     NOTE: 'text-obj' patterns for 'word' and file /path

    " 'isfname' 'isf'  string
    "   default: @,48-57,/,.,-,_,+,,,#,$,%,~,=
    if has('unix')
        set isfname=@,48-57,_,#,~,$,-,/,\\,.,+,,,%,=
    endif
    "set isfname+=@,48-57,_,#,~,$,-,/,.,+,,,%,=

    " 'isident' 'isi'  string
    "   default: @,48-57,_,192-255
    set isident+=@,48-57,_,192-255
    " 'iskeyword' 'isk'  string
    "   default: @,48-57,_
    "   custom:  @,48-57,_,192-255,:
    "   ft=help: !-~,^*,^|,^",192-255
    set iskeyword+=@,48-57,_,192-255

    if executable('rg')
        let &grepprg    = '/usr/bin/env rg -H --vimgrep --context=0'
        let &grepformat = '%f:%l:%c:%m'
    else
        let &grepprg = 'grep --color=never -e --exclude-dir .git -nrI $* . /dev/null'
    endif
" }}} Searching and Patterns


" {{{ Syntax
    syntax sync minlines=16 maxlines=512 linebreaks=1
    set synmaxcol=700
" }}} Syntax


" {{{ Clipboard
    if has('osx')
        let g:clipboard = {
            \ 'name': 'pbcopy',
            \ 'copy': {
            \    '+': 'pbcopy',
            \    '*': 'pbcopy',
            \  },
            \ 'paste': {
            \    '+': 'pbpaste',
            \    '*': 'pbpaste',
            \ },
            \ 'cache_enabled': 1,
        \ }
    elseif has('unix')
        let g:clipboard = {
            \ 'name': 'xclip',
            \ 'copy': {
            \    '+': 'xclip -selection clipboard -in',
            \    '*': 'xclip -selection clipboard -in',
            \  },
            \ 'paste': {
            \    '+': 'xclip -selection clipboard -out',
            \    '*': 'xclip -selection clipboard -out',
            \ },
            \ 'cache_enabled': 1,
        \ }
    endif
" }}} Clipboard


" {{{ Mouse stuff
    " INFO: https://www.iterm2.com/faq.html
    if has('mouse_sgr')
        set ttymouse=sgr
    endif
" }}} Syntax
