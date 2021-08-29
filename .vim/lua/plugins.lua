-- lua/plugins.lua
-- vim: fdm=marker:
require('util')

local ft = vim.fn['z#constants#globals#Ft']()
-- for k,v in pairs(ft) do print(k,v) end


-- INFO: https://github.com/wbthomason/packer.nvim
local fn = vim.fn
require('packer').startup(function(use) -- {{{
  -- integrate with other programs {{{
  use 'wbthomason/packer.nvim' -- Packer can manage itself

  use { 'Shougo/vimproc.vim', run = 'make', };
  use { 'tpope/vim-git' };
  use { 'tpope/vim-fugitive' };
  --use { 'airblade/vim-gitgutter'
  --  requires = {
  --    { 'gilligan/textobj-gitgutter', },
  --  },
  --};
  use { 'mhinz/vim-signify' };
  use { 'whiteinge/diffconflicts' };

  use { 'tmux-plugins/vim-tmux', ft={'tmux'}, };
  use { 'tmux-plugins/vim-tmux-focus-events' };
  use { 'christoomey/vim-tmux-navigator' };
  -- }}}

  -- Add features and functionality. {{{
  -- fzf
  --if has({ 'mac' };)
  --let s:fzfd = expand("$BREW/opt/fzf")
  --if !isdirectory(s:fzfd) | throw { 'Z:NotFound fzf_dir: ' };.s:fzfd | endif
  --exe "use { '".s:fzfd."', { 'do' };: { -> fzf#install } })"
  --elseif has({ 'unix') || has('win32' };)
  --if has({ 'unix') || has('win32' };)
  --use { 'junegunn/fzf', PIf(executable('fzf'), { 'do' };: { -> fzf#install() } })
  --use { 'junegunn/fzf', PIf(executable('fzf' };))
  --use { 'junegunn/fzf.vim', PIf(executable('fzf' };))
  use { 'junegunn/fzf',
    run = './install --all',
    requires = {
      {'junegunn/fzf.vim', run = ':call fzf#install()', },
      {'yuki-ycino/fzf-preview.vim', },
    },
  };
  --endif

  --use { 'easymotion/vim-easymotion' };

  -- nerdtree
  use { 'scrooloose/nerdtree',
    cmd = {'NERDTreeToggle', 'NERDTreeCWD', 'NERDTreeFromBookmark'},
    requires = {
      {'jistr/vim-nerdtree-tabs', },
      {'scrooloose/nerdcommenter', },
      {'taiansu/nerdtree-ag',         cond = "PExe('ag')", },
      {'Xuyuanp/nerdtree-git-plugin', cond = "PExe('git')", },
      {'tiagofumo/vim-nerdtree-syntax-highlight',
        requires = {{'ryanoasis/vim-devicons', }, },
      },
    },
  };

  --use { 'ms-jpq/chadtree', {'branch': 'chad', 'do': 'python3 -m chadtree deps' };}

  use { 'mbbill/undotree' };

  -- sessions
  use { 'xolox/vim-misc' };
  use { 'xolox/vim-session' };

  -- making / linting
  use { 'Shougo/neoinclude.vim' };
  use { 'neomake/neomake' };
  use { 'sbdchd/neoformat' };
  use { 'jaawerth/nrun.vim' };

  use { 'ms-jpq/coq_nvim',
    branch = 'coq',
    requires = {
      {'ms-jpq/coq.artifacts', branch = 'artifacts', },
    },
  };

  -- LSP - language server protocol
  use { 'neovim/nvim-lsp',
    requires = {
      {'kabouzeid/nvim-lspinstall', },
      {'williamboman/nvim-lsp-installer', },
      {'ray-x/lsp_signature.nvim', },
      {'RishabhRD/nvim-lsputils',
        requires = {{'RishabhRD/popfix', }, },
      },
      {'jose-elias-alvarez/nvim-lsp-ts-utils',
        requires = {{'jose-elias-alvarez/null-ls.nvim', }, },
      },
    },
  };

  use { 'lewis6991/gitsigns.nvim',
    requires = {
      {'nvim-lua/plenary.nvim', },
    };
  };

  --use { 'codota/tabnine-vim' };
  --use { 'tzachar/compe-tabnine', PIf(PHas('nvim-compe' };), {
  --\ { 'do': './install.sh' };,
  --\ })

  use { 'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate',
    requires = {
      {'tjdevries/colorbuddy.vim', },
    },
  };

  use { 'autozimu/LanguageClient-neovim',
    branch = 'next',
    run = 'bash install.sh && yarn global add flow-bin typescript',
  };
  --elseif has({ 'win32' };)
  --use { 'autozimu/LanguageClient-neovim', PIf(has('nvim' };), {
  --\ { 'branch': 'next' };,
  --\ { 'do':     'powershell -executionpolicy bypass -File install.ps1' };,
  --\ })
  --endif

  use { 'dstein64/nvim-scrollview' };

  use { 'Shougo/echodoc.vim' };
  use { 'Shougo/context_filetype.vim' };
  use { 'Shougo/neco-syntax' };
  use { 'Shougo/neco-vim' };
  --use { 'eagletmt/neco-ghc',  {'for' : ['haskell' };]}

  -- snippets
  --use { 'Shougo/neosnippet', PIf(has('nvim' };))
  --use { 'Shougo/neosnippet-snippets', PIf(has('nvim') && PHas('neosnippet' };))

  -- etc
  --use { 'ervandew/supertab' };
  use { 'tpope/vim-rsi' };
  use { 'embear/vim-localvimrc' };

  use { 'liuchengxu/vim-clap', run = ':call clap#installer#force_download()', };
  use { 'lukas-reineke/indent-blankline.nvim' };
  -- }}}

  -- Language support. {{{
    use { 'euclidianAce/BetterLua.vim' };
    use { 'chr4/nginx.vim' };
    use { 'wavded/vim-stylus', ft = ft['stylus'], };
    use { 'lumiliet/vim-twig', ft = ft['twig'], };
    --use { 'joonty/vdebug',                 {'for': ft['php' };]}
    --use { 'StanAngeloff/php.vim',          {'for': ft['php' };]}
    --use { '2072/PHP-Indenting-for-VIm',    {'for': ft['php' };]}
    --use { 'lvht/phpfold.vim',              {'for': ft['php' };]}
    --use { 'miya0001/vim-dict-wordpress',   {'for': ft['php' };]}
    --use { 'roxma/LanguageServer-php-neovim',  PIf(has('nvim'), {'do': 'composer install && composer run-script parse-stubs' };})
    use { 'lvht/phpcd.vim',
      cond = "PExe('composer')",
      ft = ft['php'],
      run = 'composer install',
    };
    use { 'othree/html5.vim' };
    use { 'lifepillar/pgsql.vim', ft = ft['sql'], };
    --use { 'sheerun/vim-polyglot' };
    --use { 'lambdalisue/vim-pyenv',         {'for': ft['py' };]}
    use { 'python-mode/python-mode', ft = ft['py'], };
    use { 'gisphm/vim-gitignore' };
    use { 'rust-lang/rust.vim',          ft = ft['rs'], };
    use { 'vim-scripts/applescript.vim', ft = ft['scpt'], };
    use { 'guns/vim-clojure-highlight',  ft = ft['clj'], };
    use { 'pangloss/vim-javascript',     ft = ft['js'], };
    use { 'leafgarland/typescript-vim',  ft = ft['ts'], };
    use { 'peitalin/vim-jsx-typescript', ft = ft['ts'], };
    use { 'prettier/vim-prettier',
      run = 'yarn install',
      ft = ccat(ft['js'], ft['ts']),
    };
    --use { 'prettier/vim-prettier',         {'do': 'yarn install' };}
        --{ 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql', 'markdown', 'vue', 'yaml', 'html' };] }
    use { 'hashivim/vim-terraform', ft = {'terraform', 'json'}, };
    --use { 'Quramy/tsuquyomi' };
    use { 'yuezk/vim-js', ft = ft['js'], };
    --use { 'HerringtonDarkholme/yats.vim' };
    use { 'maxmellon/vim-jsx-pretty',   ft = ft['js'], };
    use { 'Olical/vim-syntax-expand',   ft = ft['js'], };
    use { 'ternjs/tern_for_vim',        ft = ft['js'], run = 'npm install', };
    use { 'othree/jspc.vim',            ft = ft['js'], };
    use { 'itchyny/vim-haskell-indent', ft = {'haskell'}, };
    --use { 'flowtype/vim-flow',             {'for': ft['js' };]}
    -- markdown
    use { 'plasticboy/vim-markdown', ft = {'markdown'}, };
    use { 'nelstrom/vim-markdown-folding', ft = {'markdown'}, };
    -- web
    --use { 'saltstack/salt-vim', {'for': ['sls' };]}
    use { 'lepture/vim-jinja', ft = ft['jinja'], };
    --use { 'mattn/emmet-vim',    {'for': ft['markup'] + ft['styles'] + ['jsx', 'tsx' };]}
    use { 'mattn/emmet-vim' };
    --use { 'styled-components/vim-styled-components', { 'branch': 'main', 'for': ft['js' };] }

    -- clang
    use { 'libclang-vim/libclang-vim', ft = ft['cx'], };
    --use { 'justmao945/vim-clang',      {'for': ft['cx' };]}

    use { 'kchmck/vim-coffee-script', ft = {'coffee'}, };
    use { 'tweekmonster/braceless.vim' };
    --use { 'vim-scripts/openvpn' };
    --use { 'vim-utils/vim-man' };
    --use { 'chrisbra/csv.vim',       {'for': ['csv' };]}
    use { 'chrisbra/csv.vim' };
    use { 'cespare/vim-toml',       ft = {'toml'}, };
    use { 'digitaltoad/vim-pug',    ft = ft['jade'], };
    use { 'tpope/vim-afterimage',   ft = ft['image'], };
    use { 'kylef/apiblueprint.vim', ft = {'apiblueprint'}, };

    use { 'PProvost/vim-ps1' };
    use { 'PotatoesMaster/i3-vim-syntax', ft = 'i3', };
    --use { 'darfink/vim-plist',      {'for': ['plist' };]}
    use { 'darfink/vim-plist' };
    --use { 'PProvost/vim-ps1',       {'for': ['ps1', 'xml', 'ps1xml' };]}
    --use { 'LnL7/vim-nix' };
    use { 'alvan/vim-closetag', ft = {'javascript.jsx', 'typescript.tsx', 'html'}, };
    use { 'isobit/vim-caddyfile' };
    use { 'wgwoods/vim-systemd-syntax' }; -- systemctl / systemd
    use { 'tomlion/vim-solidity' };
  -- }}}

  -- Beautify Vim. {{{
    use { 'tjdevries/colorbuddy.vim', 
      after = {'nvim-treesitter', },
    };
    use { 'chriskempson/base16-vim' };
    use { 'vim-scripts/AfterColors.vim' };
    use { 'fedorenchik/AnsiEsc' };
    --use { 'nathanaelkane/vim-indent-guides' };
    --use { 'luochen1990/rainbow' };, PIf(v:false, {})
    --use { 'ap/vim-css-color',          { 'for': ft['styles' };] }
    use { 'arakashic/chromatica.nvim', ft = ft['cx'], };
    --use { 'qstrahl/vim-matchmaker' };
    --use { 'machakann/vim-highlightedyank', { 'on': '<Plug>(highlightedyank)' }; }
    use { 'markonm/traces.vim' };
    use {'haya14busa/incsearch.vim',
      requires = {
        {'haya14busa/incsearch-easymotion.vim', },
        {'haya14busa/incsearch-fuzzy.vim', },
      },
    };

    use { 'haya14busa/vim-keeppad' };
  -- }}}

  -- Direct text manipulation. {{{
    use { 'b4winckler/vim-angry' };
    use { 'tommcdo/vim-exchange' };
    --use { 'cohama/lexima.vim' };
    --use { 'Raimondi/delimitMate' };
    use { 'windwp/nvim-autopairs', run = ':UpdateRemotePlugins', };
    --use { 'jiangmiao/auto-pairs' };
    --use { 'wellle/targets.vim' };
    use { 'tpope/vim-surround' };

    -- textobj
    use { 'kana/vim-textobj-user' };
    use { 'kana/vim-textobj-indent' };
    use { 'kana/vim-textobj-line' };
    use { 'kana/vim-textobj-syntax' };
    --use { 'kana/vim-textobj-lastpat' };
    use { 'kana/vim-textobj-fold' };
    use { 'kana/vim-textobj-function' };
    use { 'thinca/vim-textobj-between' };
    use { 'glts/vim-textobj-comment' };
    use { 'saaguero/vim-textobj-pastedtext' };
    use { 'paulhybryant/vim-textobj-path' };
    use { 'beloglazov/vim-textobj-quotes' };
    use { 'saihoooooooo/vim-textobj-space' };
    use { 'jceb/vim-textobj-uri' };
    use { 'Julian/vim-textobj-variable-segment' };
    use { 'libclang-vim/vim-textobj-clang', ft = ft['cx'], };
    use { 'libclang-vim/vim-textobj-function-clang', ft = ft['cx'], };

    use { 'bruno-/vim-space' };
    use { 'Konfekt/FastFold' };
    use { 'Konfekt/FoldText' };
    --use { 'kopischke/vim-stay', PIf(has('nvim' };))
  -- }}}

  -- Silent enhancements. {{{
      use { 'editorconfig/editorconfig-vim' };
      use { 'kana/vim-niceblock' };
      use { 'tpope/vim-repeat' };
      use { 'vim-scripts/visualrepeat' };
      use { 'junegunn/vim-easy-align' };
      use { 'tpope/vim-sleuth' };
      use { 'sickill/vim-pasta' };
      --use { 'unblevable/quick-scope' };
      use { 'kana/vim-operator-user' };
      use { 'haya14busa/vim-operator-flashy' };
      --use { 'itchyny/vim-cursorword' };
      --use { 'itchyny/vim-parenmatch' };
      use { 'kopischke/vim-fetch' };
      use { 'pbrisbin/vim-mkdir' };
      --use { 'AndrewRadev/switch.vim' };
      use { 'vim-utils/vim-vertical-move' };
      use { 'tpope/vim-unimpaired' };
  -- }}}
end) -- }}}


-- INFO: https://github.com/savq/paq-nvim#bootstrapping {{{
  --local fn = vim.fn
  --local install_path = fn.stdpath('data') .. '/site/pack/paqs/start/paq-nvim'
  --if fn.empty(fn.glob(install_path)) > 0 then
    --fn.system({'git', 'clone', '--depth=1', 'https://github.com/savq/paq-nvim.git', install_path})
  --end

  --require 'paq' {
    --{'savq/paq-nvim'};

    -- integrate with other programs {{{
    --{'Shougo/vimproc.vim'};
    --{'rizzatti/dash.vim'};
    --{'tpope/vim-git'};
    --{'tpope/vim-fugitive'};
    --{'mhinz/vim-signify'};
    --{'whiteinge/diffconflicts'};
    --{'tmux-plugins/vim-tmux'};
    --{'tmux-plugins/vim-tmux-focus-events'};
    --{'christoomey/vim-tmux-navigator'};
    -- }}}
  --}
-- }}}
