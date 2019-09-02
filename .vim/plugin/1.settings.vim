" rc/settings
"  vim: foldmethod=marker:
scriptencoding utf-8


set nonumber numberwidth=2  " hybrid line numbers - relative & static
set title                   " file title in the $TERM titlebar
set showtabline=1           " show buffer tabs when more than `n`
set pastetoggle=<F1>        " for 'Insert' mode pasting with <S-Insert>
set history=2048            " persist mode-cmdline history
set noerrorbells visualbell " no system bell, no screen blink on error
set shell=$SHELL

set updatetime=250        " for CursorHold autocmd (milliseconds)
let g:netrw_dirhistmax=0  " http://www.vim.org/scripts/script.php?script_id=1075


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
    set wildmode=longest:full,full
    " NOTE: 'wildignore'  (default: v:null)
    set wildignore=
    set wildignore+=*.o,*.obj,*.so,*.exe,*.dll,*.manifest,*.dmg
    set wildignore+=*.swp,*.pyc,*.class
    set wildignore+=*.tar,*.bz,*.gz,*.xz,*.zip,*.7z,*.rar
    set wildignore+=*/.git/*,*/node_modules/*
    set wildignore+=*.swc,*.swc.old
    set wildignore+=*~,~*

    set pumheight=8
    set completeopt=menu,menuone,preview,noinsert,noselect
    set conceallevel=1 concealcursor=nvic

    let s:vim_data_dirs = {}
    let s:editor_name = fnamemodify($VIM, ':t')
    for [s:dir_name, s:dir_path] in items({
        \ 'undo'   : 'undo',
        \ 'swap'   : 'swap',
        \ 'backup' : 'backup',
    \ })
        "let s:dir = g:dotvim_f.'/'.s:dir_path
        let s:dir = $XDG_DATA_HOME.'/'.s:editor_name.'/'.s:dir_path
        if !isdirectory(s:dir) | call mkdir(s:dir) | endif
        let s:vim_data_dirs[s:dir_name] = s:dir
    endfor

    if has('persistent_undo')
        set undofile
        let &undodir = z#util#TempDirs('', s:vim_data_dirs['undo'])
    endif

    let &directory = z#util#TempDirs('//', s:vim_data_dirs['swap'])
    set swapfile

    let g:omni_sql_no_default_maps = 1

    let &backupdir = z#util#TempDirs('', s:vim_data_dirs['backup'])
    if has('wildignore')
        let &backupskip = &backupskip.','.s:vim_data_dirs['undo']  .'/*'
        let &backupskip = &backupskip.','.s:vim_data_dirs['swap']  .'/*'
        let &backupskip = &backupskip.','.s:vim_data_dirs['backup'].'/*'
    endif
" }}} undo / redo, swap, backup


" {{{ Moving around and editing
    set nostartofline              " Avoid moving cursor to BOL when jumping around.
    set virtualedit=all            " Let cursor move past $ in command mode.
    set backspace=indent,eol,start " Allow backspacing over autoindent, EOL, and BOL.
    set autoindent                 " Always set autoindenting on.
    set lazyredraw                 " For better macro performance.
    set synmaxcol=180
    set ttimeoutlen=30             " Time (ms) for a key code sequence to complete.
    augroup RcSettings_timeoutlen
        au!
        " The time (ms) for a mapped sequence to complete.
        autocmd InsertEnter * set timeoutlen=170
        autocmd InsertLeave * set timeoutlen=700
    augroup END
" }}} Moving around and editing


" {{{ 'cpoptions'
    "   default:aABceFs
    set cpo+=aA        " cpo-a cpo-A
    set cpo+=c cpo-=C  " cpo-c cpo-C
    set cpo+=b cpo-=B  " cpo-b cpo-B
    set cpo+=d cpo-=D  " cpo-d cpo-D
    set cpo+=Ee        " cpo-E cpo-e
    set cpo+=fF        " cpo-f cpo-F
    set cpo-=i cpo+=I  " cpo-i cpo-I
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
    "set cpo+=_         " cpo-_
" }}} 'cpoptions'


" {{{ Tiny aesthetic tweaks
    set cul cuc         " A horizontal line for the cursor location.
    set ruler           " Show the cursor position all the time.
    set scrolloff=3     " Keep n context lines above and below the cursor.
    set sidescrolloff=4 " FIXME
    set sidescroll=2    " FIXME
    set showmatch       " Briefly jump to a paren once it's balanced.
    set list            " Visually display tabs and trailing whitespace.
    set listchars=
        \eol:¬,
        \tab:→\ ,
        \trail:·,
        \extends:⟩,
        \precedes:⟨,
        \nbsp:␣
" }}} Tiny aesthetic tweaks


" {{{ Whitespace
    set expandtab       " Use spaces, not tabs, for autoindent/tab key.
    set copyindent
    set preserveindent
    set tabstop=4       " <Tab> inserts n spaces.
    set softtabstop=0   " <BS> over an autoindent deletes both spaces.
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
    set foldnestmax=2
    set foldminlines=3

    "let g:vimsyn_folding='afp'
    let g:fastfold_skip_filetypes = [
        \ 'taglist'
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
    " NOTE: default  ->  set shortmess=filnxtToO
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
    set laststatus=2     " allways show status line
    set confirm          " Y-N-C prompt if closing with unsaved changes.
    set noshowcmd        " Show incomplete normal mode commands as I type.
    set report=0         " : commands always print changed line count.
    set ruler            " Show some info, even without statuslines.
    set laststatus=2     " Always show statusline, even if only 1 window.
" }}}


" {{{ Searching and Patterns
    set ignorecase      " Default to using case insensitive searches.
    set smartcase       " unless uppercase letters are used in the regex.
    set smarttab        " Handle tabs more intelligently.
    set smartindent     " Indent intelligently.
    set hlsearch        " Highlight searches by default.
    set incsearch       " Incrementally search while typing a /regex.
    set regexpengine=0  " Auto-switch regexp engines if workload hangs.

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
    set  isfname=@,48-57,_,#,~,$,-,/,\\,.,+,,,%,=
    "set isfname+=@,48-57,_,#,~,$,-,/,.,+,,,%,=

    " 'isident' 'isi'  string
    "   default: @,48-57,_,192-255
    set isident+=@,48-57,_,192-255

    " 'iskeyword' 'isk'  string
    "   default: @,48-57,_
    set iskeyword+=@,48-57,_,192-255
" }}} Searching and Patterns


" {{{ Syntax
    syntax sync minlines=256 linebreaks=1
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
        \ 'cache_enabled': 0,
    \ }
endif
" }}} Syntax
