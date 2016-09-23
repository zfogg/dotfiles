" rc/settings
scriptencoding utf-8


filetype plugin indent on


set number numberwidth=3    " hybrid line numbers - relative & static
set title                   " file title in the $TERM titlebar
set showtabline=1           " show buffer tabs when more than `n`
set pastetoggle=<F1>        " for 'Insert' mode pasting with <S-Insert>
set history=8192            " persist mode-cmdline history
set noerrorbells visualbell " no system bell, no screen blink on error
set shell=$SHELL
set wildmenu                " custom completion menu
set wildmode=
    \longest:full,
    \list:full


set omnifunc=syntaxcomplete#Complete


set sessionoptions-=blank,buffers " The 'Session' plugin does this on save.
set updatetime=650 " for CursorHold autocmd (milliseconds)


let g:netrw_dirhistmax=0 " http://www.vim.org/scripts/script.php?script_id=1075


" {{{ undo / redo, swap, backup
    let s:dotvim_dotdirs = {}
    for [s:dir_name, s:dir_path] in items({
        \ 'undo'   : '.undo',
        \ 'swap'   : '.swap',
        \ 'backup' : '.backup',
    \ })
        let s:dir = g:dotvim_f.'/'.s:dir_path
        if !isdirectory(s:dir) | call mkdir(s:dir) | endif
        let s:dotvim_dotdirs[s:dir_name] = s:dir[1:]
    endfor

    " wildignore
    set wildignore+=*.o,*.obj,*.so,,*.exe,*.dll,*.manifest,*.dmg
    set wildignore+=*.swp,*.pyc,*.class
    set wildignore+=*.tar,*.bz,*.gz,*.xz,*.zip
    set wildignore+=*~

    " {un,re}do history
    let &undodir = z#util#TempDirs('/', '', s:dotvim_dotdirs['undo'])
    set undofile

    " swapfiles
    let &directory = z#util#TempDirs('/', '//', s:dotvim_dotdirs['swap'])

    " backups
    let &backupdir  = z#util#TempDirs('/', '', s:dotvim_dotdirs['backup'])
    let &backupskip = z#util#TempDirs('/', '/*',
        \ s:dotvim_dotdirs['undo'],
        \ s:dotvim_dotdirs['swap'],
        \ s:dotvim_dotdirs['backup'],
    \ )
    set backupskip=&backupskip
" }}} undo / redo, swap, backup


" {{{ Moving around and editing
    set nostartofline   " Avoid moving cursor to BOL when jumping around.
    set virtualedit=all " Let cursor move past $ in command mode.
    set backspace=2     " Allow backspacing over autoindent, EOL, and BOL.
    set autoindent      " Always set autoindenting on.
    set lazyredraw      " For better macro performance.
" }}} Moving around and editing


" {{{ Tiny aesthetic tweaks
    set cursorline      " A horizontal line for the cursor location.
    set ruler           " Show the cursor position all the time.
    set scrolloff=3     " Keep n context lines above and below the cursor.
    set sidescrolloff=5 " FIXME
    set sidescroll=1    " FIXME
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


" Whitespace {{{
    set expandtab       " Use spaces, not tabs, for autoindent/tab key.
    set copyindent
    set preserveindent
    set softtabstop=4   " <BS> over an autoindent deletes both spaces.
    set shiftwidth=4    " An indent level is n spaces.
    set tabstop=4       " <Tab> inserts n spaces.
    set shiftround      " Rounds indent to a multiple of shiftwidth.
    set nowrap          " Don't wrap text.
    set linebreak       " Don't wrap textin the middle of a word.
    set wrapmargin=0    " Seriously, don't break lines.
    set textwidth=0     " Don't break lines.
    set showbreak=↪\    " Visualize wrapped lines.
" }}}


" Folding. {{{
    set foldmethod=syntax
    set foldopen=mark,percent,quickfix,tag,undo
    set foldenable
" }}}


" Reading and writing. {{{
    set noautowrite      " Never write a file unless I request it.
    set noautowriteall   " NEVER.
    set noautoread       " Don't automatically re-read changed files.
    set modeline         " Allow vim options to be embedded in files;
    set modelines=3      " they must be within the first or last 5 lines.
    set ffs=unix,dos,mac " Try recognizing dos, unix, and mac line endings.
" }}}


" Messages, info, and statuses. {{{
    set shortmess=aOstTI " :help 'shortmess'
    set laststatus=2     " allways show status line
    set confirm          " Y-N-C prompt if closing with unsaved changes.
    set showcmd          " Show incomplete normal mode commands as I type.
    set report=0         " : commands always print changed line count.
    set ruler            " Show some info, even without statuslines.
    set laststatus=2     " Always show statusline, even if only 1 window.
    "set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P
" }}}


" Searching and Patterns {{{
    set ignorecase " Default to using case insensitive searches.
    set smartcase  " unless uppercase letters are used in the regex.
    set smarttab   " Handle tabs more intelligently.
    set hlsearch   " Highlight searches by default.
    set incsearch  " Incrementally search while typing a /regex.

    " 'text-obj' patterns for 'word' and file /path
        " NOTE: http://www.ascii-code.com/
            " '@'       == /[a-zA-Z]/ == filter(isalpha, $ascii_arr)
            " ,48-57,   == /[0-9]/    == filter(isdigit, $ascii_arr)
            " ,192-255, == /[À-ÿ]/    /* accented characters - extended ascii */
            " ,,,       == /,/
            " ,@-@,     == /@/
            " ,^,,      == /[^,]/
            " ,^<EOL>   == /\^/
        " NOTE: :h 'isfname' | :help 'iskeyword'
    set   isfname=@,48-57,_,#,~,$,-,/,.,+,,,%,=
    set iskeyword=@,48-57,_,192-255
" }}}
