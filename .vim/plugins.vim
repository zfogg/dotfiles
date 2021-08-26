" rc/plugins
scriptencoding utf-8


func! PIf(cond, ...) abort
    let l:opts = get(a:000, 0, {})
    let l:cond = get(a:, 'cond', v:false)
    return l:cond ? l:opts : extend(l:opts, { 'on': [], 'for': [] })
endfunc

func! PHas(...) abort
    return call(function('z#util#HasPlugin'), a:000)
endfunc


let ft = copy(z#constants#globals#Ft())


call plug#begin('~/.vim/bundle')

Plug 'junegunn/vim-plug'

" integrate with other programs {{{
    Plug 'Shougo/vimproc.vim', { 'do': 'make' }
    "Plug 'rizzatti/dash.vim'
    Plug 'tpope/vim-git'
    Plug 'tpope/vim-fugitive', PIf(executable('git'), {})
    "Plug 'airblade/vim-gitgutter'
    Plug 'mhinz/vim-signify'
    Plug 'whiteinge/diffconflicts'

    Plug 'tmux-plugins/vim-tmux', PIf(executable('tmux'), { 'for': 'tmux' })
    Plug 'tmux-plugins/vim-tmux-focus-events', PIf(exists('$TMUX'))
    Plug 'christoomey/vim-tmux-navigator',     PIf(exists('$TMUX'))

    "Plug 'rfratto/vim-iterm2-navigator', PIf(!empty($ITERM_SESSION_ID) && isdirectory('/Applications/iTerm.app'), {'do': 'make install'})
" }}}


" Add features and functionality. {{{
    " fzf
    "if has('mac')
        "let s:fzfd = expand("$BREW/opt/fzf")
        "if !isdirectory(s:fzfd) | throw 'Z:NotFound fzf_dir: '.s:fzfd | endif
        "exe "Plug '".s:fzfd."', { 'do': { -> fzf#install } })"
    "elseif has('unix') || has('win32')
    if has('unix') || has('win32')
        "Plug 'junegunn/fzf', PIf(executable('fzf'), { 'do': { -> fzf#install() } })
        "Plug 'junegunn/fzf', PIf(executable('fzf'))
        "Plug 'junegunn/fzf.vim', PIf(executable('fzf'))
        Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
        Plug 'junegunn/fzf.vim', PIf(PHas('fzf'), {})
        Plug 'yuki-ycino/fzf-preview.vim', PIf(PHas('fzf'), {})
    endif

    "Plug 'easymotion/vim-easymotion'

    Plug 'ryanoasis/vim-devicons'
    " nerdtree
    Plug 'scrooloose/nerdtree',         { 'on': ['NERDTreeToggle', 'NERDTreeCWD', ], }
    Plug 'jistr/vim-nerdtree-tabs',     PIf(PHas('nerdtree'), {})
    Plug 'taiansu/nerdtree-ag',         PIf(PHas('nerdtree') && executable('ag'), {})
    Plug 'Xuyuanp/nerdtree-git-plugin', PIf(PHas('nerdtree') && executable('git'), {})
    Plug 'tiagofumo/vim-nerdtree-syntax-highlight', PIf(
                \PHas('nerdtree') && PHas('vim-devicons'), {})

    "Plug 'ms-jpq/chadtree', {'branch': 'chad', 'do': 'python3 -m chadtree deps'}

    Plug 'mbbill/undotree'

    " sessions
    Plug 'xolox/vim-misc'
    Plug 'xolox/vim-session'
    " making / linting
    Plug 'Shougo/neoinclude.vim'
    Plug 'neomake/neomake', PIf(has('nvim'))
    Plug 'sbdchd/neoformat', PIf(has('nvim'))
    Plug 'jaawerth/nrun.vim'

    " completion
    "Plug 'hrsh7th/nvim-compe',       PIf(has('nvim'))
    "Plug 'andersevenrud/compe-tmux', PIf(PHas('nvim-compe'))
    "Plug 'tamago324/compe-zsh',      PIf(PHas('nvim-compe'))
    "Plug 'nvim-lua/plenary.nvim',    PIf(PHas('nvim-compe'))
    "Plug 'Shougo/deol.nvim',         PIf(PHas('nvim-compe')) " recommended to use together.

    Plug 'ms-jpq/coq_nvim',      PIf(has('nvim'), {'branch': 'coq'})
    Plug 'ms-jpq/coq.artifacts', PIf(PHas('coq_nvim'), {'branch': 'artifacts'})

    " LSP - language server protocol
    Plug 'neovim/nvim-lsp',                 PIf(has('nvim'))
    Plug 'neovim/nvim-lspconfig',           PIf(PHas('nvim-lsp'))
    "Plug 'kabouzeid/nvim-lspinstall',       PIf(PHas('nvim-lsp'))
    Plug 'williamboman/nvim-lsp-installer', PIf(PHas('nvim-lsp') && PHas('nvim-lspconfig'))
    Plug 'ray-x/lsp_signature.nvim',        PIf(has('nvim'))
    Plug 'onsails/lspkind-nvim',            PIf(has('nvim'))
    Plug 'RishabhRD/popfix',                PIf(has('nvim'))
    Plug 'RishabhRD/nvim-lsputils',         PIf(has('nvim'))

    Plug 'nvim-lua/plenary.nvim',                PIf(has('nvim'))
    Plug 'lewis6991/gitsigns.nvim',              PIf(has('nvim'))
    Plug 'jose-elias-alvarez/null-ls.nvim',      PIf(has('nvim'))
    Plug 'jose-elias-alvarez/nvim-lsp-ts-utils', PIf(PHas('nvim-lsp'))

    "Plug 'codota/tabnine-vim'
    "Plug 'tzachar/compe-tabnine', PIf(PHas('nvim-compe'), {
        "\ 'do': './install.sh',
    "\ })

    Plug 'nvim-treesitter/nvim-treesitter', PIf(has('nvim'), {'do': ':TSUpdate'}) " We recommend updating the parsers on update

    if has('unix')
        Plug 'autozimu/LanguageClient-neovim', PIf(has('nvim'), {
            \ 'branch': 'next',
            \ 'do':     'bash install.sh && npm install -g flow-bin typescript',
        \ })
        "\ 'do':     'bash install.sh',
    elseif has('win32')
        Plug 'autozimu/LanguageClient-neovim', PIf(has('nvim'), {
            \ 'branch': 'next',
            \ 'do':     'powershell -executionpolicy bypass -File install.ps1',
        \ })
    endif
    "Plug 'Shougo/denite.nvim', PIf(has('nvim'), {'do': ':UpdateRemotePlugins'})

    " deoplete
    "Plug 'roxma/nvim-completion-manager', PIf(has('nvim'))
    "if has('nvim')
        "Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    "else
        "Plug 'Shougo/deoplete.nvim'
        "Plug 'roxma/nvim-yarp'
        "Plug 'roxma/vim-hug-neovim-rpc'
    "endif
    "Plug 'zchee/deoplete-zsh',        PIf(PHas('deoplete.nvim'),                    {'for': ['zsh']})
    "Plug 'zchee/deoplete-go',         PIf(PHas('deoplete.nvim'),                    {'for': ['go'], 'do': 'make'})
    "Plug 'zchee/deoplete-jedi',       PIf(PHas('deoplete.nvim')  && has('python3'), {'for': ft['py']})
    "Plug 'zchee/deoplete-clang',      PIf(PHas('deoplete.nvim')  && has('unix'),    {'for': ft['cx'], })
    "Plug 'carlitux/deoplete-ternjs',  PIf(PHas('deoplete.nvim'),                    {'for': ft['js'], })
    "Plug 'fszymanski/deoplete-emoji', PIf(PHas('deoplete.nvim')  && has('mac'))
    "Plug 'wokalski/autocomplete-flow', PIf(PHas('deoplete.nvim'))
    "if has('win32') || has('win64')
        "Plug 'tbodt/deoplete-tabnine', {'do': 'powershell.exe .\install.ps1'}
    "else
        "Plug 'tbodt/deoplete-tabnine', {'do': './install.sh'}
    "endif
    "Plug 'deoplete-plugins/deoplete-lsp', PIf((PHas('nvim-lsp') && PHas('deoplete.nvim')), {
        "\ 'do': 'GO111MODULE=on go get golang.org/x/tools/gopls@latest',
    "\ })

    "Plug 'racer-rust/vim-racer', { 'for': ['rust'], }

    Plug 'dstein64/nvim-scrollview', PIf(has('nvim'))

    Plug 'Shougo/echodoc.vim'

    Plug 'Shougo/context_filetype.vim'
    Plug 'Shougo/neco-syntax'
    Plug 'Shougo/neco-vim'
    "Plug 'eagletmt/neco-ghc',  {'for' : ['haskell']}

    " snippets
    "Plug 'Shougo/neosnippet', PIf(has('nvim'))
    "Plug 'Shougo/neosnippet-snippets', PIf(has('nvim') && PHas('neosnippet'))

    " etc
    "Plug 'ervandew/supertab'
    Plug 'tpope/vim-rsi'
    Plug 'embear/vim-localvimrc'

    Plug 'liuchengxu/vim-clap', { 'do': { -> clap#installer#force_download() } }
    Plug 'lukas-reineke/indent-blankline.nvim', PIf(has('nvim'))

    "Plug 'nvim-treesitter/nvim-treesitter', { 'do' : ':TSUpdate' }

    "Plug 'liuchengxu/vim-which-key', PIf(v:false, {'on': ['WhichKey', 'WhichKey!']})
" }}}


" Language support. {{{
    Plug 'euclidianAce/BetterLua.vim'
    Plug 'chr4/nginx.vim'
    Plug 'wavded/vim-stylus',             {'for': ft['stylus']}
    Plug 'lumiliet/vim-twig',             {'for': ft['twig']}
    "Plug 'joonty/vdebug',                 {'for': ft['php']}
    "Plug 'StanAngeloff/php.vim',          {'for': ft['php']}
    "Plug '2072/PHP-Indenting-for-VIm',    {'for': ft['php']}
    "Plug 'lvht/phpfold.vim',              {'for': ft['php']}
    "Plug 'miya0001/vim-dict-wordpress',   {'for': ft['php']}
    "Plug 'roxma/LanguageServer-php-neovim',  PIf(has('nvim'), {'do': 'composer install && composer run-script parse-stubs'})
    Plug 'lvht/phpcd.vim', PIf(has('nvim'), {'for': ft['php'], 'do': 'composer install'})
    Plug 'othree/html5.vim'
    Plug 'lifepillar/pgsql.vim',          {'for': ft['sql']}
    "Plug 'sheerun/vim-polyglot'
    "Plug 'lambdalisue/vim-pyenv',         {'for': ft['py']}
    Plug 'python-mode/python-mode',       {'for': ft['py']}
    Plug 'gisphm/vim-gitignore'
    Plug 'rust-lang/rust.vim',            {'for': ft['rs']}
    Plug 'vim-scripts/applescript.vim',   {'for': ft['scpt']}
    Plug 'guns/vim-clojure-highlight',    {'for': ft['clj']}
    Plug 'pangloss/vim-javascript',       {'for': ft['js']}
    Plug 'leafgarland/typescript-vim',    {'for': ft['ts']}
    Plug 'peitalin/vim-jsx-typescript',   {'for': ft['ts']}
    Plug 'prettier/vim-prettier',         {'do': 'yarn install', 'for': ft['js'] + ft['ts']}
    "Plug 'prettier/vim-prettier',         {'do': 'yarn install'}
        "'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql', 'markdown', 'vue', 'yaml', 'html'] }
    Plug 'hashivim/vim-terraform',        {'for': ['terraform', 'json']}
    "Plug 'Quramy/tsuquyomi'
    Plug 'yuezk/vim-js',                  {'for': ft['js']}
    "Plug 'HerringtonDarkholme/yats.vim'
    Plug 'maxmellon/vim-jsx-pretty',      {'for': ft['js']}
    Plug 'Olical/vim-syntax-expand',      {'for': ft['js']}
    Plug 'ternjs/tern_for_vim',           {'for': ft['js'], 'do': 'npm install'}
    Plug 'othree/jspc.vim',               {'for': ft['js']}
    Plug 'itchyny/vim-haskell-indent',    {'for': ['haskell'] }
    "Plug 'flowtype/vim-flow',             {'for': ft['js']}
    " markdown
    Plug 'plasticboy/vim-markdown',            {'for': ['markdown']}
    Plug 'nelstrom/vim-markdown-folding', {'for': ['markdown']}
    " web
    "Plug 'saltstack/salt-vim', {'for': ['sls']}
    Plug 'lepture/vim-jinja',  {'for': ft['jinja']}
    "Plug 'mattn/emmet-vim',    {'for': ft['markup'] + ft['styles'] + ['jsx', 'tsx']}
    Plug 'mattn/emmet-vim'
    "Plug 'styled-components/vim-styled-components', { 'branch': 'main', 'for': ft['js'] }

    " clang
    Plug 'libclang-vim/libclang-vim', {'for': ft['cx']}
    "Plug 'justmao945/vim-clang',      {'for': ft['cx']}

    Plug 'kchmck/vim-coffee-script', {'for': ['coffee']}
    Plug 'tweekmonster/braceless.vim',
        \{'for': ft['py'] + ft['jade'] + ['coffee', 'yaml', 'haml']}
    "Plug 'vim-scripts/openvpn'
    "Plug 'vim-utils/vim-man'
    "Plug 'chrisbra/csv.vim',       {'for': ['csv']}
    Plug 'chrisbra/csv.vim'
    Plug 'cespare/vim-toml',       {'for': ['toml']}
    Plug 'digitaltoad/vim-pug',    {'for': ft['jade']}
    Plug 'tpope/vim-afterimage',   {'for': ft['image']}
    Plug 'kylef/apiblueprint.vim', {'for': ['apiblueprint']}

    Plug 'PProvost/vim-ps1'
    Plug 'PotatoesMaster/i3-vim-syntax', {'for': 'i3'}
    "Plug 'darfink/vim-plist',      {'for': ['plist']}
    Plug 'darfink/vim-plist'
    "Plug 'PProvost/vim-ps1',       {'for': ['ps1', 'xml', 'ps1xml']}
    "Plug 'LnL7/vim-nix'
    Plug 'alvan/vim-closetag' ",     {'for': ['javascript.jsx', 'typescript.tsx', 'html']}
    Plug 'isobit/vim-caddyfile'
    Plug 'wgwoods/vim-systemd-syntax' " systemctl / systemd
    Plug 'tomlion/vim-solidity' " ethereum's solidity
" }}}


" Beautify Vim. {{{
    Plug 'chriskempson/base16-vim'
    Plug 'vim-scripts/AfterColors.vim'
    Plug 'fedorenchik/AnsiEsc'
    "Plug 'nathanaelkane/vim-indent-guides'
    "Plug 'luochen1990/rainbow', PIf(v:false, {})
    "Plug 'ap/vim-css-color',          { 'for': ft['styles'] }
    Plug 'arakashic/chromatica.nvim', { 'for': ft['cx']  }
    "Plug 'qstrahl/vim-matchmaker'
    "Plug 'machakann/vim-highlightedyank', { 'on': '<Plug>(highlightedyank)' }

    Plug 'markonm/traces.vim'
    Plug 'haya14busa/incsearch.vim',            PIf(v:true, {})
    Plug 'haya14busa/incsearch-easymotion.vim', PIf(PHas('incsearch.vim'), {})
    Plug 'haya14busa/incsearch-fuzzy.vim',      PIf(PHas('incsearch.vim'), {})

    Plug 'haya14busa/vim-keeppad'
" }}}


" Direct text manipulation. {{{
    Plug 'b4winckler/vim-angry'
    Plug 'tommcdo/vim-exchange'
    "Plug 'cohama/lexima.vim'
    "Plug 'Raimondi/delimitMate'
    if has('nvim')
        Plug 'windwp/nvim-autopairs', { 'do': ':UpdateRemotePlugins' }
    else
        Plug 'jiangmiao/auto-pairs'
    endif
    Plug 'jiangmiao/auto-pairs'
    Plug 'scrooloose/nerdcommenter'
    "Plug 'wellle/targets.vim'
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
    Plug 'saaguero/vim-textobj-pastedtext'
    Plug 'paulhybryant/vim-textobj-path'
    Plug 'beloglazov/vim-textobj-quotes'
    Plug 'saihoooooooo/vim-textobj-space'
    Plug 'jceb/vim-textobj-uri'
    Plug 'Julian/vim-textobj-variable-segment'
    "Plug 'libclang-vim/vim-textobj-clang', {'for': ft['cx']}
    "Plug 'libclang-vim/vim-textobj-function-clang', {'for': ft['cx']}
    "Plug 'gilligan/textobj-gitgutter', PIf(PHas('vim-gitgutter') && PHas('vim-textobj-user'), {})

    Plug 'bruno-/vim-space'
    Plug 'Konfekt/FastFold'
    Plug 'Konfekt/FoldText'
    "Plug 'kopischke/vim-stay', PIf(has('nvim'))
" }}}


" Silent enhancements. {{{
    Plug 'editorconfig/editorconfig-vim'
    Plug 'kana/vim-niceblock'
    Plug 'tpope/vim-repeat'
    Plug 'vim-scripts/visualrepeat'
    Plug 'junegunn/vim-easy-align'
    Plug 'tpope/vim-sleuth'
    Plug 'sickill/vim-pasta'
    "Plug 'unblevable/quick-scope'
    Plug 'kana/vim-operator-user'
    Plug 'haya14busa/vim-operator-flashy'
    "Plug 'itchyny/vim-cursorword'
    "Plug 'itchyny/vim-parenmatch'
    Plug 'kopischke/vim-fetch'
    Plug 'pbrisbin/vim-mkdir'
    "Plug 'AndrewRadev/switch.vim'
    Plug 'vim-utils/vim-vertical-move'
    Plug 'tpope/vim-unimpaired'
" }}}


call plug#end()
