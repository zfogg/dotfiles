-- lua/zfogg/plugins.lua
-- vim: fdm=marker:
require 'zfogg.util'

local fn = vim.fn
local ft = fn['z#constants#globals#Ft']()
-- for k,v in pairs(ft) do print(k,v) end

local install_path = vim.fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.execute('!git clone https://github.com/wbthomason/packer.nvim '..install_path)
end


local packer_startup_opts = {
  config = {
    display = {
      open_fn = require('packer.util').float,
    },
  },
}

require('packer').startup({function(use) -- {{{
    use { 'lewis6991/impatient.nvim',
      config = [[
      require('impatient')
      --require('impatient').enable_profile()
      ]],
    };

    use { 'b0o/mapx.nvim',
      config = [[require('rc.mapx')]],
    };
  -- integrate with other programs {{{
    use 'wbthomason/packer.nvim' -- Packer can manage itself

    --use { 'Shougo/vimproc.vim', run = 'make', };
    --use { 'tpope/vim-git' };
    use { 'tpope/vim-fugitive' };
    --use { 'airblade/vim-gitgutter'
    --  requires = {
    --    { 'gilligan/textobj-gitgutter', },
    --  },
    --};
    use { 'whiteinge/diffconflicts' };

    use { 'tmux-plugins/vim-tmux', ft={'tmux'}, };
    use { 'tmux-plugins/vim-tmux-focus-events',
      enable=(not vim.g.vscode and vim.fn.has('nvim')),
    };
    use { 'christoomey/vim-tmux-navigator',
      enable=(not vim.g.vscode and vim.fn.has('nvim')),
    };

    use { 'Olical/vim-enmasse', opt=true, cmd='EnMasse', };
    use { 'kevinhwang91/nvim-bqf', };
  -- }}}

  -- Add features and functionality. {{{
    --use { 'glepnir/dashboard-nvim',
      --setup = (function()
        --vim.cmd [[ let g:dashboard_default_executive = 'telescope' ]]
      --end),
    --};

    use { 'lewis6991/gitsigns.nvim',
      setup = [[
        vim.cmd('let b:rcplugin_gitsigns = 1')
      ]],
      config = [[
        require('rc.gitsigns')
      ]],
      requires = {
        { 'nvim-lua/plenary.nvim', },
      },
    };

    -- Search
    use {
      { 'nvim-telescope/telescope.nvim',
        setup = [[require('rc.telescope_setup')]],
        config = [[require('rc.telescope')]],
        opt = true,
        cmd = 'Telescope',
        module = 'telescope',
        requires = {
          { 'nvim-lua/popup.nvim', opt=true, },
          { 'nvim-lua/plenary.nvim', opt=true, },
          { 'telescope-frecency.nvim',  opt=true, },
          { 'telescope-fzf-native.nvim',  opt=true, },
        },
        wants = {
          'popup.nvim',
          'telescope-frecency.nvim',
          'telescope-fzf-native.nvim',
        },
      },
      { 'nvim-telescope/telescope-frecency.nvim',
        after = 'telescope.nvim',
        requires = 'tami5/sql.nvim', },
      { 'nvim-telescope/telescope-fzf-native.nvim',
        run = 'make', },
    }

    -- Project Management/Sessions
    use {
      'dhruvasagar/vim-prosession',
      after = 'vim-obsession',
      requires = { { 'tpope/vim-obsession', opt=true, cmd='Prosession' } },
      --config = [[require('config.prosession')]],
    }

    use { 'AndrewRadev/bufferize.vim', };

    use { 'dstein64/vim-startuptime',
      cmd='StartupTime',
      opt=true,
      config = [[vim.g.startuptime_tries = 10]],
    };

    use { 'folke/lua-dev.nvim', ft = 'lua', };

    -- Undo tree
    use {
      'mbbill/undotree',
      opt = true, cmd = 'UndotreeToggle',
      config = [[vim.g.undotree_SetFocusWhenToggle = 1]],
    }

    use { 'junegunn/fzf',
      run = './install --all',
      requires = {
        {'junegunn/fzf.vim', run = ':call fzf#install()', },
        {'yuki-ycino/fzf-preview.vim', },
      },
    };
    --endif

    --use { 'easymotion/vim-easymotion' };

    --use { 'romgrk/barbar.nvim',
      --requires = {'kyazdani42/nvim-web-devicons'},
    --};

    use { 'voldikss/vim-floaterm' };

    -- fern
    use { 'lambdalisue/fern.vim',
      --opt = true, cmd = { 'Fern', 'FernDo', },
      requires = {
        {'antoinemadec/FixCursorHold.nvim', },
        {'lambdalisue/fern-git-status.vim', },
        {'lambdalisue/fern-hijack.vim', },
        {'yuki-yano/fern-preview.vim', },
        {'lambdalisue/fern-renderer-nerdfont.vim', requires={
          {'lambdalisue/nerdfont.vim', },
          {'lambdalisue/glyph-palette.vim', }, },
        },
        {'lambdalisue/fern-ssh', },
        {'lambdalisue/fern-mapping-project-top.vim', },
        {'hrsh7th/fern-mapping-collapse-or-leave.vim', },
        {'LumaKernel/fern-mapping-fzf.vim', enable=false, requires={
          'junegunn/fzf', }, },
      },
    };

    use { 'scrooloose/nerdcommenter', };

    --use { 'ms-jpq/chadtree', {'branch': 'chad', 'do': 'python3 -m chadtree deps' };}

    -- sessions
    use { 'xolox/vim-misc' };

    use { 'ms-jpq/coq_nvim',
      branch = 'coq',
      run = 'python3 -m coq deps',
      requires = {
        {'ms-jpq/coq.artifacts', branch='artifacts', },
        {'ms-jpq/coq.thirdparty', branch='3p', },
        {'neovim/nvim-lspconfig', },
      },
    };

    use { 'jubnzv/virtual-types.nvim',
      requires = {
        { 'neovim/nvim-lspconfig', },
      },
    };

    -- LSP - language server protocol
    use {'neovim/nvim-lspconfig',
      config = [[require('rc.lspconfig')]],
      requires = {
        {'ray-x/lsp_signature.nvim',
          config = [[require('rc.lsp_signature')]], },
        {'onsails/lspkind-nvim', },
        {'kosayoda/nvim-lightbulb', },
        {'RishabhRD/nvim-lsputils',
          requires = {'RishabhRD/popfix', }, },
        {'jose-elias-alvarez/null-ls.nvim',
          config = [[require('rc.null-ls')]],
          requires = { 'nvim-lua/plenary.nvim', },
        }
      },
    };

    use { 'weilbith/nvim-code-action-menu',
      opt=true, cmd = 'CodeActionMenu',
    };

    use { 'williamboman/nvim-lsp-installer',
      config = [[require('rc.lsp_installer')]],
      requires = { 'neovim/nvim-lspconfig', },
    };

    --use { 'jackguo380/vim-lsp-cxx-highlight',
      --requires = {
        --{ 'neovim/nvim-lsp', },
      --}, };

    use { 'nvim-treesitter/nvim-treesitter',
      run = ':TSUpdate',
      config = [[require('rc.treesitter')]],
      requires = {
        {'tjdevries/colorbuddy.vim', },
      },
    };

    --use { 'autozimu/LanguageClient-neovim',
      --branch = 'next',
      --run = 'bash install.sh',
    --};

    --use { 'dstein64/nvim-scrollview' };

    --use { 'Shougo/echodoc.vim',
    --  config = [[
    --    vim.cmd('set noshowmode')
    --    vim.cmd('let g:echodoc#enable_at_startup=1')
    --  ]]
    --};
    use { 'Shougo/context_filetype.vim' };
    --use { 'Shougo/neco-syntax' };
    --use { 'Shougo/neco-vim' };
    --use { 'eagletmt/neco-ghc',  {'for' : ['haskell' };]}

    -- etc
    --use { 'ervandew/supertab' };
    use { 'tpope/vim-rsi' };
    use { 'embear/vim-localvimrc' };

    use { 'liuchengxu/vim-clap', run = ':call clap#installer#download_binary()', };
    use { 'lukas-reineke/indent-blankline.nvim',
      --config=[[ ]],
    };
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
    --use { 'lvht/phpcd.vim',
      --cond = "PExe('composer')",
      --ft = ft['php'], run = 'composer install',
    --};
    use { 'othree/html5.vim' };
    use { 'lifepillar/pgsql.vim', ft = ft['sql'], };
    --use { 'sheerun/vim-polyglot' };
    --use { 'lambdalisue/vim-pyenv',         {'for': ft['py' };]}
    use { 'python-mode/python-mode', ft = ft['py'], };
    use { 'gisphm/vim-gitignore' };
    use { 'rust-lang/rust.vim',          ft = ft['rs'], };
    use { 'vim-scripts/applescript.vim', ft = ft['scpt'], };
    use { 'guns/vim-clojure-highlight',  ft = ft['clj'], };
    --use { 'pangloss/vim-javascript',     ft = ft['js'], };
    --use { 'maxmellon/vim-jsx-pretty', ft = ccat(ft['jsx'], ft['tsx']) };
    use { 'yuezk/vim-js', };
    use { 'HerringtonDarkholme/yats.vim', };
    use { 'maxmellon/vim-jsx-pretty',
      --ft = ccat(ft['jsx'], ft['tsx']),
      requires = {
        { 'yuezk/vim-js', },
        { 'HerringtonDarkholme/yats.vim', },
      },
      config = [[
        vim.g.vim_jsx_pretty_colorful_config = 1
      ]],
    };

    --use { 'HerringtonDarkholme/yats.vim',  ft = ft['ts'], };
    --use { 'leafgarland/typescript-vim',  ft = ft['ts'], };
    --use { 'peitalin/vim-jsx-typescript', ft = ft['tsx'], };

    --use { 'prettier/vim-prettier', ft = ccat(ft['js'], ft['ts']), run = 'yarn install', };
    --use { 'prettier/vim-prettier',         {'do': 'yarn install' };}
        --{ 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql', 'markdown', 'vue', 'yaml', 'html' };] }
    use { 'hashivim/vim-terraform', ft = {'terraform', 'json'}, };
    --use { 'Quramy/tsuquyomi' };
    --use { 'yuezk/vim-js', ft = ft['js'], };
    --use { 'HerringtonDarkholme/yats.vim' };
    --use { 'maxmellon/vim-jsx-pretty',   ft = ft['jsx'], };
    use { 'Olical/vim-syntax-expand',   ft = ft['js'], };
    --use { 'ternjs/tern_for_vim',        ft = ft['js'], run = 'npm install', };
    --use { 'othree/jspc.vim',            ft = ft['js'], };
    use { 'itchyny/vim-haskell-indent', ft = {'haskell'}, };
    --use { 'flowtype/vim-flow',             {'for': ft['js' };]}
    -- markdown
    use { 'plasticboy/vim-markdown', ft = {'markdown'}, };
    use { 'nelstrom/vim-markdown-folding', ft = {'markdown'}, };
    -- web
    use { 'saltstack/salt-vim', ft = {'sls', }, };
    use { 'lepture/vim-jinja', ft = ft['jinja'], };
    use { 'mattn/emmet-vim' };
    --use { 'styled-components/vim-styled-components', { 'branch': 'main', 'for': ft['js' };] }

    -- clang
    use { 'libclang-vim/libclang-vim', ft = ft['cx'], };

    use { 'kchmck/vim-coffee-script', ft = {'coffee'}, };
    use { 'tweekmonster/braceless.vim' };
    use { 'chrisbra/csv.vim',       ft = {'csv',}, };
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
    use { 'alvan/vim-closetag', ft = {'javascriptreact', 'typescriptreact', 'html'}, };
    use { 'isobit/vim-caddyfile' };
    use { 'wgwoods/vim-systemd-syntax' }; -- systemctl / systemd
    use { 'tomlion/vim-solidity' };
  -- }}}

  -- Beautify Vim. {{{
    use { 'nvim-lualine/lualine.nvim',
      config = [[require('rc.lualine')]],
      requires = { 'kyazdani42/nvim-web-devicons', opt = true, },
    };
    use { 'akinsho/bufferline.nvim',
      config = [[require('rc.bufferline')]],
      requires = 'kyazdani42/nvim-web-devicons',
    };
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
    use { 'windwp/nvim-autopairs',
      config = [[require('rc.autopairs')]],
    };
    --use { 'jiangmiao/auto-pairs' };
    --use { 'wellle/targets.vim', };
    use { 'tpope/vim-surround', };

    -- textobj
    use { 'kana/vim-textobj-user',
      requires = {
        { 'kana/vim-textobj-indent' },
        { 'kana/vim-textobj-line' },
        { 'kana/vim-textobj-syntax' },
        { 'kana/vim-textobj-lastpat' },
        { 'kana/vim-textobj-fold' },
        { 'kana/vim-textobj-function' },
        { 'thinca/vim-textobj-between' },
        { 'glts/vim-textobj-comment' },
        { 'saaguero/vim-textobj-pastedtext' },
        { 'paulhybryant/vim-textobj-path' },
        { 'beloglazov/vim-textobj-quotes' },
        { 'saihoooooooo/vim-textobj-space' },
        { 'jceb/vim-textobj-uri' },
        { 'Julian/vim-textobj-variable-segment' },
        { 'libclang-vim/vim-textobj-clang', ft = ft['cx'], },
        { 'libclang-vim/vim-textobj-function-clang', ft = ft['cx'], },
      },
    };

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
      --use { 'tpope/vim-unimpaired' };
  -- }}}

  end,
  config = {
    display = {
      open_fn = function()
        return require('packer.util').float({ border = 'single' })
      end,
    },
}, }) -- }}}


vim.cmd([[
aug rc_packer_plugins
  au!
  "au User PackerComplete    call z#util#Helptags() | UpdateRemotePlugins
  "au User PackerCompileDone call z#util#Helptags()
  " INFO: https://github.com/wbthomason/packer.nvim/#quickstart
  "au BufWritePost plugins.lua source <afile> | echom expand('PackerCompile...') | PackerCompile
  au BufWritePost plugins.lua source <afile> | PackerCompile
aug END
]])
