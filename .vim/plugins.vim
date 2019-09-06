" rc/plugins
scriptencoding utf-8


func! PIf(cond, ...) abort
    let l:opts = get(a:000, 0, {})
    let l:cond = get(a:, 'cond', v:false)
    return l:cond ? l:opts : extend(l:opts, { 'on': [], 'for': [] })
endfunc


let ft = copy(z#constants#globals#Ft())


call plug#begin('~/.vim/bundle')

" integrate with other programs {{{
    Plug 'Shougo/vimproc.vim', { 'do': 'make' }
    Plug 'rizzatti/dash.vim'
    Plug 'tpope/vim-fugitive'
    Plug 'airblade/vim-gitgutter'
    Plug 'tmux-plugins/vim-tmux', { 'for': 'tmux' }
    Plug 'tmux-plugins/vim-tmux-focus-events', PIf(executable('tmux') && !empty($TMUX))
    Plug 'christoomey/vim-tmux-navigator',     PIf(executable('tmux') && !empty($TMUX))
" }}}


" Add features and functionality. {{{
    " fzf
    "Plug $BREW.'/opt/fzf', PIf(executable('fzf') && isdirectory($BREW.'/opt/fzf'),
            "\{'dir': $BREW.'/opt/fzf', 'do': './install --all'})
    "Plug 'junegunn/fzf.vim'
    "Plug 'easymotion/vim-easymotion'
    " nerdtree
    Plug 'scrooloose/nerdtree'
    Plug 'Xuyuanp/nerdtree-git-plugin'
    Plug 'taiansu/nerdtree-ag'
    Plug 'jistr/vim-nerdtree-tabs'
    Plug 'tiagofumo/vim-nerdtree-syntax-highlight'

    " sessions
    Plug 'xolox/vim-misc'
    Plug 'xolox/vim-session'
    " making / linting
    Plug 'Shougo/neoinclude.vim'
    Plug 'neomake/neomake', PIf(has('nvim'))
    Plug 'jaawerth/nrun.vim'

    " completion
    "Plug 'autozimu/LanguageClient-neovim', PIf(has('nvim'), {'do': ':UpdateRemotePlugins'})
    "Plug 'roxma/LanguageServer-php-neovim',  PIf(has('nvim'), {'do': 'composer install && composer run-script parse-stubs'})
    "Plug 'lvht/phpcd.vim', PIf(has('nvim'), {'for': ft['php'], 'do': 'composer install'})
    Plug 'Shougo/denite.nvim', PIf(has('nvim'), {'do': ':UpdateRemotePlugins'})

	" deoplete
    Plug 'Shougo/deoplete.nvim', PIf(has('nvim'), {'do': ':UpdateRemotePlugins'})
    "Plug 'roxma/nvim-completion-manager', PIf(has('nvim'))
    Plug 'Shougo/echodoc.vim'
    Plug 'zchee/deoplete-zsh',  {'for': ['zsh']}
    Plug 'zchee/deoplete-go',   {'for': ['go'], 'do': 'make'}
    Plug 'zchee/deoplete-jedi', PIf(has('python3'), {'for': ft['py']})
    Plug 'zchee/deoplete-clang', PIf(has('unix'), {'for': ft['cx'], })
    Plug 'racer-rust/vim-racer', {'for': ['rust'], }
    Plug 'carlitux/deoplete-ternjs', {'for': ft['js'], }
    Plug 'fszymanski/deoplete-emoji', PIf(has('mac'))
    Plug 'Shougo/context_filetype.vim'
    Plug 'Shougo/neco-syntax'
    Plug 'Shougo/neco-vim',     {'for': ['vim']}
    Plug 'eagletmt/neco-ghc',  {'for' : ['haskell'], }

    " snippets
    Plug 'Shougo/neosnippet', PIf(has('nvim'))
    Plug 'Shougo/neosnippet-snippets', PIf(has('nvim'))

    " etc
    Plug 'ervandew/supertab'
    Plug 'tpope/vim-rsi'
    "Plug 'miya0001/vim-dict-wordpress',   {'for': ft['php']}
    Plug 'MarcWeber/vim-addon-local-vimrc'
" }}}


" Language support. {{{
    Plug 'chr4/nginx.vim'
    Plug 'wavded/vim-stylus',             {'for': ft['stylus']}
    Plug 'lumiliet/vim-twig',             {'for': ft['twig']}
    Plug 'joonty/vdebug',                 {'for': ft['php']}
    "Plug 'StanAngeloff/php.vim',          {'for': ft['php']}
    "Plug '2072/PHP-Indenting-for-VIm',    {'for': ft['php']}
    Plug 'lvht/phpfold.vim',              {'for': ft['php']}
    Plug 'othree/html5.vim'
    Plug 'lifepillar/pgsql.vim',          {'for': ft['sql']}
    "Plug 'sheerun/vim-polyglot'
    Plug 'lambdalisue/vim-pyenv',         {'for': ft['py']}
    "Plug 'python-mode/python-mode',       {'for': ft['py']}
    Plug 'gisphm/vim-gitignore'
    Plug 'rust-lang/rust.vim',            {'for': ft['rs'], }
    Plug 'vim-scripts/applescript.vim',   {'for': ft['scpt']}
    Plug 'guns/vim-clojure-highlight',    {'for': ft['clj']}
    Plug 'pangloss/vim-javascript',       {'for': ft['js']}
	Plug 'Wolfy87/vim-syntax-expand'
    Plug 'ternjs/tern_for_vim',           {'for': ft['js'], 'do': 'npm install'}
    Plug 'othree/jspc.vim',               {'for': ft['js']}
    Plug 'itchyny/vim-haskell-indent',    {'for': ['haskell'] }
    " markdown
    Plug 'plasticboy/vim-markdown',            {'for': ['markdown']}
	Plug 'nelstrom/vim-markdown-folding', {'for': ['markdown']}
    " web
    Plug 'saltstack/salt-vim', {'for': ['sls']}
    Plug 'lepture/vim-jinja',  {'for': ft['jinja']}
    Plug 'mattn/emmet-vim',    {'for': ft['markup'] + ft['styles']}

    " clang
    Plug 'libclang-vim/libclang-vim', {'for': ft['cx']}
    Plug 'justmao945/vim-clang',      {'for': ft['cx']}
    Plug 'kana/vim-textobj-user'
    Plug 'libclang-vim/vim-textobj-clang', {'for': ft['cx']}
    "Plug 'libclang-vim/vim-textobj-function-clang', {'for': ft['cx']}

    Plug 'kchmck/vim-coffee-script', {'for': ['coffee']}
    Plug 'tweekmonster/braceless.vim',
        \{'for': ft['py'] + ft['jade'] + ['coffee', 'yaml', 'haml']}
    Plug 'vim-scripts/openvpn'
    "Plug 'vim-utils/vim-man'
    Plug 'chrisbra/csv.vim',    {'for': ['csv']}
    Plug 'cespare/vim-toml',    {'for': ['toml']}
    Plug 'digitaltoad/vim-pug', {'for': ft['jade']}
    Plug 'tpope/vim-afterimage', {'for': ft['image']}
    Plug 'kylef/apiblueprint.vim', {'for': ['apiblueprint']}
    Plug 'darfink/vim-plist'
" }}}


" Beautify Vim. {{{
    Plug 'chriskempson/base16-vim'
    Plug 'vim-scripts/AfterColors.vim'
    Plug 'nathanaelkane/vim-indent-guides'
    Plug 'luochen1990/rainbow', PIf(v:false, {})
    Plug 'ap/vim-css-color',          { 'for': ft['styles'] }
    Plug 'arakashic/chromatica.nvim', { 'for': ft['cx']  }
    "Plug 'qstrahl/vim-matchmaker'
    "Plug 'machakann/vim-highlightedyank', { 'on': '<Plug>(highlightedyank)' }
    Plug 'ryanoasis/vim-devicons'
    Plug 'haya14busa/incsearch.vim'
    Plug 'haya14busa/incsearch-easymotion.vim'
    Plug 'haya14busa/incsearch-fuzzy.vim'
    Plug 'haya14busa/vim-keeppad'
" }}}


" Direct text manipulation. {{{
    Plug 'b4winckler/vim-angry'
    Plug 'tommcdo/vim-exchange'
    "Plug 'cohama/lexima.vim'
    "Plug 'Raimondi/delimitMate'
    Plug 'jiangmiao/auto-pairs'
    Plug 'scrooloose/nerdcommenter'
    Plug 'wellle/targets.vim'
    Plug 'tpope/vim-surround'

	" textobj
    Plug 'kana/vim-textobj-user'
    Plug 'kana/vim-textobj-indent'
    Plug 'kana/vim-textobj-line'
    Plug 'kana/vim-textobj-syntax'
    Plug 'kana/vim-textobj-lastpat'
    Plug 'kana/vim-textobj-fold'
    Plug 'kana/vim-textobj-function'
    Plug 'thinca/vim-textobj-between'
    Plug 'glts/vim-textobj-comment'
    Plug 'gilligan/textobj-gitgutter'
    Plug 'saaguero/vim-textobj-pastedtext'
    Plug 'paulhybryant/vim-textobj-path'
    Plug 'beloglazov/vim-textobj-quotes'
    Plug 'saihoooooooo/vim-textobj-space'
    Plug 'jceb/vim-textobj-uri'
    Plug 'Julian/vim-textobj-variable-segment'
    " //textobj
    Plug 'bruno-/vim-space'
    Plug 'Konfekt/FastFold'
    Plug 'Konfekt/FoldText'
    Plug 'kopischke/vim-stay', PIf(has('nvim'))
" }}}


" Silent enhancements. {{{
    Plug 'editorconfig/editorconfig-vim'
    Plug 'kana/vim-niceblock'
    "Plug 'tpope/vim-repeat' | Plug 'vim-scripts/visualrepeat'
    Plug 'junegunn/vim-easy-align'
    Plug 'tpope/vim-sleuth'
    Plug 'sickill/vim-pasta'
    "Plug 'unblevable/quick-scope'
    Plug 'kana/vim-operator-user'
    Plug 'haya14busa/vim-operator-flashy'
    Plug 'itchyny/vim-cursorword'
    Plug 'itchyny/vim-parenmatch'
    Plug 'kopischke/vim-fetch'
    Plug 'pbrisbin/vim-mkdir'
    Plug 'AndrewRadev/switch.vim'
    Plug 'bruno-/vim-vertical-move'
" }}}

call plug#end()
