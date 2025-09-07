-- lua/zfogg/plugins.lua
-- vim: fdm=marker ft=lua:
require 'zfogg.util'

local fn = vim.fn
local ft = fn['z#constants#globals#Ft']()

-- Install lazy.nvim if not present
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

local inTmux              = string.len(vim.env.TMUX or '') > 0
local inKitty             = string.len(vim.env.TERM or '') > 0 and (vim.env.TERM:match("-kitty$") == "-kitty")
local inVscode            = not (vim.g.vscode == nil)

-- {{{ plugin list
require("lazy").setup({
  -- {{{ Do me first!
  { 'lewis6991/impatient.nvim',
    config = function()
      require('impatient')
      --require('impatient').enable_profile()
    end,
  },

  -- mapx.nvim removed - repository archived, using native vim.keymap.set instead
  -- }}}

  -- {{{ Integrate with other programs
  { 'tpope/vim-fugitive' },
  { 'whiteinge/diffconflicts' },

  { 'tmux-plugins/vim-tmux', ft = { 'tmux' } },
  { 'tmux-plugins/vim-tmux-focus-events',
    enabled = not (vim.g.vscode and not vim.fn.has('nvim')),
  },

  { 'christoomey/vim-tmux-navigator',
    init = function()
      if inVscode or inKitty or (not inTmux) then
        vim.g.tmux_navigator_no_mappings = 1
      end
    end,
  },

  { 'knubie/vim-kitty-navigator',
    enabled = not ((not inKitty) or inTmux),
  },

  { 'Olical/vim-enmasse',
    cmd = 'EnMasse',
  },
  { 'kevinhwang91/nvim-bqf' },
  -- }}}

  -- {{{ Add features and functionality.
  { 'dense-analysis/ale',
    init = function() require('rc.ale').setup() end,
    config = function() require('rc.ale').config() end,
  },

  { 'simrat39/symbols-outline.nvim',
    init = function() require('rc.symbols-outline').setup() end,
    config = function() require('rc.symbols-outline').config() end,
  },

  { 'lewis6991/gitsigns.nvim',
    init = function()
      vim.cmd('let b:rcplugin_gitsigns = 1')
    end,
    config = function()
      require('rc.gitsigns')
    end,
    dependencies = {
      'nvim-lua/plenary.nvim',
    },
  },

  -- Search
  { 'nvim-telescope/telescope.nvim',
    init = function() require('rc.telescope').setup() end,
    config = function() require('rc.telescope').config() end,
    cmd = 'Telescope',
    dependencies = {
      'nvim-lua/popup.nvim',
      'nvim-lua/plenary.nvim',
    },
  },
  { 'nvim-telescope/telescope-frecency.nvim',
    config = function() require("telescope").load_extension("frecency") end,
    dependencies = { "kkharji/sqlite.lua", "nvim-telescope/telescope.nvim" },
  },
  { 'nvim-telescope/telescope-fzf-native.nvim',
    build = 'make',
    config = function() require("telescope").load_extension("fzf") end,
    dependencies = { "nvim-telescope/telescope.nvim" },
  },
  { 'xiyaowong/telescope-emoji.nvim',
    config = function() require("telescope").load_extension("emoji") end,
    dependencies = { "nvim-telescope/telescope.nvim" },
  },

  -- Project Management/Sessions
  { 'tpope/vim-obsession',
    cmd = 'Prosession',
  },
  { 'dhruvasagar/vim-prosession',
    dependencies = { 'tpope/vim-obsession' },
  },

  { 'AndrewRadev/bufferize.vim' },

  { 'dstein64/vim-startuptime',
    cmd = 'StartupTime',
    lazy = true,
    config = function() vim.g.startuptime_tries = 10 end,
  },

  { 'folke/neodev.nvim',
    ft = 'lua',
    config = function()
      require('neodev').setup({})
    end,
    dependencies = {
      'nvim-treesitter/nvim-treesitter',
      'nvim-lua/plenary.nvim',
      'nvim-telescope/telescope.nvim',
      'j-hui/fidget.nvim',
    },
  },

  -- Undo tree
  { 'mbbill/undotree',
    cmd = 'UndotreeToggle',
    config = function() vim.g.undotree_SetFocusWhenToggle = 1 end,
  },

  { 'junegunn/fzf',
    build = './install --all',
    dependencies = {
      { 'junegunn/fzf.vim', build = ':call fzf#install()' },
      'yuki-ycino/fzf-preview.vim',
    },
  },

  { 'voldikss/vim-floaterm' },

  -- fern
  { 'lambdalisue/fern.vim',
    init = function() require("rc.fern").setup() end,
    config = function() require("rc.fern").config() end,
    dependencies = {
      'antoinemadec/FixCursorHold.nvim',
      'lambdalisue/fern-git-status.vim',
      'lambdalisue/fern-hijack.vim',
      'yuki-yano/fern-preview.vim',
      { 'lambdalisue/fern-renderer-nerdfont.vim',
        dependencies = {
          'lambdalisue/nerdfont.vim',
          'lambdalisue/glyph-palette.vim',
        },
      },
      'lambdalisue/fern-ssh',
      'lambdalisue/fern-mapping-project-top.vim',
      'hrsh7th/fern-mapping-collapse-or-leave.vim',
      { 'LumaKernel/fern-mapping-fzf.vim',
        enabled = false,
        dependencies = { 'junegunn/fzf' },
      },
    },
  },

  { 'preservim/nerdcommenter' },

  -- sessions
  { 'xolox/vim-misc' },

  { 'lukas-reineke/lsp-format.nvim' },

  { 'github/copilot.vim',
    enabled = false,
    init = function() require("rc.copilot").setup() end,
    config = function() require("rc.copilot").config() end,
  },

  { 'ms-jpq/coq_nvim',
    build = ':COQdeps',
    init = function() require("rc.coq").setup() end,
    config = function() require("rc.coq").config() end,
    dependencies = {
      { 'ms-jpq/coq.artifacts', branch = 'artifacts' },
    },
  },

  { 'jubnzv/virtual-types.nvim',
    dependencies = {
      'neovim/nvim-lspconfig',
    },
  },

  { 'j-hui/fidget.nvim',
    config = function() require("fidget").setup({}) end,
  },

  { 'williamboman/mason.nvim',
    init = function() require('rc.mason').setup() end,
    config = function() require('rc.mason').config() end,
    dependencies = {
      'williamboman/mason-lspconfig.nvim',
      'neovim/nvim-lspconfig',
      'ray-x/lsp_signature.nvim',
      'ms-jpq/coq_nvim',
      'folke/neodev.nvim',
      'lukas-reineke/lsp-format.nvim',
    },
  },

  { 'williamboman/mason-lspconfig.nvim' },

  -- LSP - language server protocol
  { 'neovim/nvim-lspconfig',
    config = function() require('rc.lspconfig') end,
    dependencies = {
      'onsails/lspkind-nvim',
      'kosayoda/nvim-lightbulb',
      { 'RishabhRD/nvim-lsputils',
        dependencies = { 'RishabhRD/popfix' },
      },
    },
  },

  { 'ray-x/lsp_signature.nvim',
    init = function() require('rc.lsp_signature').setup() end,
    config = function() require('rc.lsp_signature').config() end,
  },

  { 'weilbith/nvim-code-action-menu',
    cmd = 'CodeActionMenu',
  },

  { 'nvim-treesitter/nvim-treesitter',
    build = ':TSUpdate',
    config = function() require('rc.treesitter') end,
    dependencies = {
      'tjdevries/colorbuddy.vim',
      { 'windwp/nvim-ts-autotag',
        config = function() require('nvim-ts-autotag').setup { enable = true } end,
      },
    },
  },

  { 'Shougo/context_filetype.vim' },
  { 'embear/vim-localvimrc' },

  { 'liuchengxu/vim-clap',
    build = ':call clap#installer#download_binary()',
  },
  { 'lukas-reineke/indent-blankline.nvim',
    init = function() require('rc.indent-blankline').setup() end,
    config = function() require('rc.indent-blankline').config() end,
  },
  -- }}}

  -- {{{ Language support.
  { 'euclidianAce/BetterLua.vim' },
  { 'chr4/nginx.vim' },
  { 'wavded/vim-stylus', ft = ft['stylus'] },
  { 'lumiliet/vim-twig', ft = ft['twig'] },
  { 'othree/html5.vim' },
  { 'lifepillar/pgsql.vim', ft = ft['sql'] },
  { 'sheerun/vim-polyglot' },
  { 'python-mode/python-mode', ft = ft['py'] },
  { 'gisphm/vim-gitignore' },
  { 'rust-lang/rust.vim', ft = ft['rs'] },
  { 'vim-scripts/applescript.vim', ft = ft['scpt'] },
  { 'guns/vim-clojure-highlight', ft = ft['clj'] },
  { 'leafgarland/typescript-vim' },
  { 'peitalin/vim-jsx-typescript',
    dependencies = {
      'leafgarland/typescript-vim',
    },
  },
  { 'fladson/vim-kitty' },
  { 'hashivim/vim-terraform', ft = { 'terraform', 'json' } },
  { 'Olical/vim-syntax-expand', ft = ft['js'] },
  { 'itchyny/vim-haskell-indent', ft = { 'haskell' } },
  -- markdown
  { 'preservim/vim-markdown', ft = { 'markdown' } },
  { 'MeanderingProgrammer/render-markdown.nvim', ft = { 'markdown' } },
  { 'dhruvasagar/vim-table-mode',
    dependencies = {
      'godlygeek/tabular',
    },
    ft = { 'markdown' },
  },
  { 'nelstrom/vim-markdown-folding', ft = { 'markdown' } },
  -- web
  { 'saltstack/salt-vim', ft = { 'sls' } },
  { 'lepture/vim-jinja', ft = ft['jinja'] },
  { 'mattn/emmet-vim' },

  -- clang
  { 'libclang-vim/libclang-vim', ft = ft['cx'] },

  { 'kchmck/vim-coffee-script', ft = { 'coffee' } },
  { 'tweekmonster/braceless.vim' },
  { 'chrisbra/csv.vim', ft = { 'csv' } },
  { 'cespare/vim-toml', ft = { 'toml' } },
  { 'digitaltoad/vim-pug', ft = ft['jade'] },
  { 'tpope/vim-afterimage', ft = ft['image'] },
  { 'kylef/apiblueprint.vim', ft = { 'apiblueprint' } },

  { 'PProvost/vim-ps1', ft = { 'ps1', 'ps1xml' } },
  { 'PotatoesMaster/i3-vim-syntax', ft = { 'i3' } },
  { 'darfink/vim-plist' },
  { 'alvan/vim-closetag',
    init = function()
      vim.g.closetag_filenames = '*.html,*.xhtml,*.phtml,*.jsx,*.tsx'
      vim.g.closetag_xhtml_filenames = '*.xhtml,*.jsx,*.tsx'
      vim.g.closetag_filetypes = 'html,xhtml,phtml,javascriptreact,typescriptreact'
      vim.g.closetag_xhtml_filetypes = 'xhtml,javascriptreact,typescriptreact'
      vim.g.closetag_emptyTags_caseSensitive = 1
      vim.g.closetag_shortcut = '>'
      vim.g.closetag_close_shortcut = '<leader>>'
    end,
    ft = { 'javascriptreact', 'typescriptreact', 'html' },
  },

  { 'RRethy/nvim-treesitter-endwise',
    event = 'InsertEnter',
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
  },
  { 'windwp/nvim-ts-autotag',
    event = 'InsertEnter',
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
    config = function()
      require('nvim-ts-autotag').setup { enable = true }
    end,
  },

  { 'isobit/vim-caddyfile' },
  { 'wgwoods/vim-systemd-syntax' },
  { 'vyperlang/vim-vyper' },
  -- }}}

  -- {{{ Beautify Vim.
  { 'nvim-lualine/lualine.nvim',
    config = function() require('rc.lualine') end,
    dependencies = {
      'kyazdani42/nvim-web-devicons',
    },
  },
  { 'akinsho/bufferline.nvim',
    config = function() require('rc.bufferline') end,
    dependencies = { 'kyazdani42/nvim-web-devicons' },
  },
  { 'tjdevries/colorbuddy.vim',
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
  },
  { 'chriskempson/base16-vim' },
  { 'EdenEast/nightfox.nvim',
    init = function() require('rc.nightfox').setup() end,
    config = function() require('rc.nightfox').config() end,
  },
  { 'vim-scripts/AfterColors.vim' },
  { 'fedorenchik/AnsiEsc' },
  { 'arakashic/chromatica.nvim', ft = ft['cx'] },
  { 'markonm/traces.vim' },
  { 'haya14busa/incsearch.vim',
    dependencies = {
      'haya14busa/incsearch-easymotion.vim',
      'haya14busa/incsearch-fuzzy.vim',
    },
  },

  { 'haya14busa/vim-keeppad' },
  -- }}}

  -- {{{ Direct text manipulation.
  { 'b4winckler/vim-angry' },
  { 'tommcdo/vim-exchange' },
  { 'windwp/nvim-autopairs',
    dependencies = {
      'nvim-treesitter/nvim-treesitter',
      'ms-jpq/coq_nvim',
    },
    init = function() require('rc.autopairs').setup() end,
    config = function() require('rc.autopairs').config() end,
  },
  { 'tpope/vim-surround' },

  -- textobj
  { 'kana/vim-textobj-user',
    dependencies = {
      'kana/vim-textobj-indent',
      'kana/vim-textobj-line',
      'kana/vim-textobj-syntax',
      'kana/vim-textobj-lastpat',
      'kana/vim-textobj-fold',
      'kana/vim-textobj-function',
      'thinca/vim-textobj-between',
      'glts/vim-textobj-comment',
      'saaguero/vim-textobj-pastedtext',
      'paulhybryant/vim-textobj-path',
      'beloglazov/vim-textobj-quotes',
      'saihoooooooo/vim-textobj-space',
      'jceb/vim-textobj-uri',
      'Julian/vim-textobj-variable-segment',
      { 'libclang-vim/vim-textobj-clang', ft = ft['cx'] },
      { 'libclang-vim/vim-textobj-function-clang', ft = ft['cx'] },
    },
  },

  { 'bruno-/vim-space' },
  { 'Konfekt/FastFold' },
  { 'Konfekt/FoldText' },
  -- }}}

  -- {{{ Silent enhancements.
  { 'editorconfig/editorconfig-vim' },
  { 'kana/vim-niceblock' },
  { 'tpope/vim-repeat' },
  { 'vim-scripts/visualrepeat' },
  { 'junegunn/vim-easy-align' },
  -- { 'tpope/vim-sleuth' }, -- Removed: conflicts with vim-polyglot's built-in sleuth
  { 'kana/vim-operator-user' },
  { 'haya14busa/vim-operator-flashy',
    dependencies = { 'kana/vim-operator-user' },
  },
  { 'kopischke/vim-fetch' },
  { 'pbrisbin/vim-mkdir' },
  { 'vim-utils/vim-vertical-move' },

  { "monkoose/neocodeium",
    event = "VimEnter",
    config = function()
      local neocodeium = require("neocodeium")
      neocodeium.setup()
      vim.keymap.set("i", "<Tab>", neocodeium.accept)
    end,
  },

  -- }}}
}, {
  ui = {
    border = "single",
  },
}) -- }}}

-- {{{ autocommand to reload config
vim.cmd([[
aug rc_lazy_plugins
  au!
  au BufWritePost plugins.lua source <afile>
aug END
]])
-- }}}