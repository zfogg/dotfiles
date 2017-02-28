" rc/plugins
scriptencoding utf-8


func! PIf(cond, ...) abort
    let l:opts = get(a:000, 0, {})
    let l:cond = get(a:, 'cond', v:false)
    return l:cond ? l:opts : extend(l:opts, { 'on': [], 'for': [] })
endfunc


call plug#begin('~/.vim/bundle')


" integrate with other programs {{{
    Plug 'Shougo/vimproc.vim', { 'do': 'make' }
    "Plug 'rking/ag.vim', PIf(executable('ag'), { 'on': 'Ag' })
    Plug 'rizzatti/dash.vim'
    Plug 'tpope/vim-fugitive'
    Plug 'airblade/vim-gitgutter'
    Plug 'tmux-plugins/vim-tmux', { 'for': 'tmux' }
    Plug 'tmux-plugins/vim-tmux-focus-events', PIf(executable('tmux') && !empty($TMUX))
    Plug 'christoomey/vim-tmux-navigator',     PIf(executable('tmux') && !empty($TMUX))
" }}}


" Add features and functionality. {{{
    " fzf
    Plug $BREW.'/opt/fzf', PIf(executable('fzf') && isdirectory($BREW.'/opt/fzf'),
            \{'dir': $BREW.'/opt/fzf', 'do': './install --all'}) |
        \Plug 'junegunn/fzf.vim'
    Plug 'easymotion/vim-easymotion'
    " nerdtree
    Plug 'scrooloose/nerdtree'              |
        \Plug 'Xuyuanp/nerdtree-git-plugin' |
        \Plug 'taiansu/nerdtree-ag'         |
        \Plug 'jistr/vim-nerdtree-tabs'     |
        \Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
    " sessions
    Plug 'xolox/vim-misc' |
        \Plug 'xolox/vim-session'
    " making / linting
    Plug 'Shougo/neoinclude.vim' |
        \Plug 'neomake/neomake', PIf(has('nvim'), {'do': 'npm i -g jsonlint'})
    " completion
    Plug 'Shougo/deoplete.nvim', PIf(has('nvim'), {
            \'do' : ':UpdateRemotePlugins'
        \}) |
        \Plug 'Shougo/neco-syntax'                                  |
        \Plug 'Shougo/neco-vim',     {'for': ['vim']}               |
        \Plug 'zchee/deoplete-zsh',  {'for': ['zsh']}               |
        \Plug 'zchee/deoplete-go',   {'for': ['go'],
                \'do': 'go get -u github.com/nsf/gocode && make'}   |
        \Plug 'zchee/deoplete-jedi', {'for': ['python', 'python3']} |
        \Plug 'zchee/deoplete-clang', {'for': ['c', 'cpp',
                                              \'objc', 'objcpp']}   |
        \Plug 'racer-rust/vim-racer', {
            \'for': ['rust'],
            \'do' : 'rustup run stable cargo install racer', } |
        \Plug 'carlitux/deoplete-ternjs', {
            \'for': ['javascript', 'javascript.jsx'],
            \'do' : 'npm i -g tern', } |
        \Plug 'eagletmt/neco-ghc',  {'for' : ['haskell'] } |
        \Plug 'Shougo/context_filetype.vim'
    " snippets
    Plug 'Shougo/neosnippet' |
        \Plug 'Shougo/neosnippet-snippets'
    " etc
    Plug 'ervandew/supertab'
    Plug 'tpope/vim-rsi'
" }}}


" Language support. {{{
    Plug 'lambdalisue/vim-pyenv'
    Plug 'gisphm/vim-gitignore'
    Plug 'rust-lang/rust.vim'
    "Plug 'sheerun/vim-polyglot'
    Plug 'applescript.vim',            {'for': ['applescript', 'osascript']}
    Plug 'guns/vim-clojure-highlight', {'for': ['clojure', 'clojurescript']}
    Plug 'pangloss/vim-javascript',    {'for': ['javascript', 'javascript.jsx']} |
        \Plug 'Wolfy87/vim-syntax-expand'
    Plug 'ternjs/tern_for_vim', {'for': ['javascript', 'javascript.jsx'],
                                \'do': 'npm install'}
    Plug 'othree/jspc.vim',            {'for': ['javascript', 'javascript.jsx']}
    Plug 'itchyny/vim-haskell-indent', {'for': ['haskell'] }
    " markdown
    Plug 'plasticboy/vim-markdown',            {'for': ['markdown']} |
        \Plug 'nelstrom/vim-markdown-folding', {'for': ['markdown']}
    " web
    Plug 'saltstack/salt-vim', {'for': ['sls']}
    Plug 'lepture/vim-jinja',  {'for': ['jinja', 'jinja.html', 'sls']}
    Plug 'mattn/emmet-vim',    {'for': ['xml', 'html', 'jinja.html',
                                       \'css', 'sass', 'scss', 'less']
    \}
    " clang
    Plug 'justmao945/vim-clang',      {'for': ['c', 'cpp']} |
    Plug 'libclang-vim/libclang-vim', {'for': ['c', 'cpp']} |
        \Plug 'kana/vim-textobj-user'                                          |
        \Plug 'libclang-vim/vim-textobj-clang', {'for': ['c', 'cpp']}
    "\Plug 'libclang-vim/vim-textobj-function-clang', {'for': ['c', 'cpp']}
    Plug 'kchmck/vim-coffee-script', {'for': ['coffee']}
    Plug 'tweekmonster/braceless.vim',
        \{'for': ['coffee',
            \'python', 'python3',
            \'yaml', 'haml',
        \]}
    Plug 'openvpn'
    Plug 'vim-utils/vim-man'
    Plug 'chrisbra/csv.vim'
    Plug 'cespare/vim-toml',    {'for': ['toml']}
    Plug 'digitaltoad/vim-pug', {'for': ['jade', 'pug']}
    Plug 'tpope/vim-afterimage'
    Plug 'kylef/apiblueprint.vim', {'for': ['apiblueprint']}
    "Plug 'tpope/vim-afterimage', {'for': ['ico', 'png', 'gif']}
" }}}


" Beautify Vim. {{{
    Plug 'chriskempson/base16-vim' |
        \ Plug 'AfterColors.vim'
    Plug 'nathanaelkane/vim-indent-guides'
    Plug 'luochen1990/rainbow'
    Plug 'AnsiEsc.vim'
    Plug 'ap/vim-css-color',          { 'for': ['css', 'scss', 'sass', 'less'] }
    Plug 'arakashic/chromatica.nvim', { 'for': ['c', 'cpp', 'objc', 'objcpp']  }
    "Plug 'qstrahl/vim-matchmaker'
    "Plug 'machakann/vim-highlightedyank', { 'on': '<Plug>(highlightedyank)' }
    Plug 'ryanoasis/vim-devicons'
    "Plug 'haya14busa/incsearch.vim' |
        \Plug 'haya14busa/incsearch-easymotion.vim' |
        \Plug 'haya14busa/incsearch-fuzzy.vim'
    Plug 'haya14busa/vim-keeppad'
" }}}


" Direct text manipulation. {{{
    Plug 'b4winckler/vim-angry'
    Plug 'junegunn/vim-easy-align'
    Plug 'tommcdo/vim-exchange'
    "Plug 'cohama/lexima.vim'
    "Plug 'Raimondi/delimitMate'
    Plug 'jiangmiao/auto-pairs'
    Plug 'scrooloose/nerdcommenter'
    Plug 'wellle/targets.vim'
    Plug 'tpope/vim-surround'
    Plug 'kana/vim-textobj-user'                    |
        \Plug 'kana/vim-textobj-indent'             |
        \Plug 'kana/vim-textobj-line'               |
        \Plug 'kana/vim-textobj-syntax'             |
        \Plug 'kana/vim-textobj-lastpat'            |
        \Plug 'kana/vim-textobj-fold'               |
        \Plug 'kana/vim-textobj-function'           |
        \Plug 'thinca/vim-textobj-between'          |
        \Plug 'glts/vim-textobj-comment'            |
        \Plug 'gilligan/textobj-gitgutter'          |
        \Plug 'saaguero/vim-textobj-pastedtext'     |
        \Plug 'paulhybryant/vim-textobj-path'       |
        \Plug 'beloglazov/vim-textobj-quotes'       |
        \Plug 'saihoooooooo/vim-textobj-space'      |
        \Plug 'jceb/vim-textobj-uri'                |
        \Plug 'Julian/vim-textobj-variable-segment'
    Plug 'bruno-/vim-space'
    Plug 'Konfekt/FastFold' |
        Plug 'Konfekt/FoldText'
    Plug 'kopischke/vim-stay', PIf(has('nvim'))
" }}}


" Silent enhancements. {{{
    Plug 'editorconfig/editorconfig-vim'
    Plug 'kana/vim-niceblock'
    Plug 'tpope/vim-repeat'
    Plug 'sickill/vim-pasta'
    Plug 'unblevable/quick-scope'
    Plug 'kana/vim-operator-user' |
        Plug 'haya14busa/vim-operator-flashy'
    Plug 'itchyny/vim-cursorword'
    Plug 'itchyny/vim-parenmatch'
    Plug 'kopischke/vim-fetch'
    Plug 'pbrisbin/vim-mkdir'
    Plug 'AndrewRadev/switch.vim'
    Plug 'bruno-/vim-vertical-move'
" }}}


call plug#end()
