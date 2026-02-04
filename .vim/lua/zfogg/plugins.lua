-- lua/zfogg/plugins.lua
-- vim: fdm=marker ft=lua:
require 'zfogg.util'

local fn = vim.fn
local ft = fn['z#constants#globals#Ft']()


-- Comprehensive file types for lazy loading LSP/treesitter
local code_filetypes = vim.iter({
  ft['py'],        -- python, python3
  ft['js'],        -- javascript  
  ft['jsx'],       -- javascript.jsx, javascriptreact
  ft['ts'],        -- typescript
  ft['tsx'],       -- typescript.tsx, typescriptreact
  ft['cx'],        -- c, cpp, objc, objcpp, ch, m, mm, h, hpp
  ft['rs'],        -- rust
  ft['vim'],       -- vim, vimscript
  ft['config'],    -- toml, yaml, yml, json, jsonc, json5, markdown, md, apiblueprint, ini, cfg, conf, env
  ft['go'],        -- go, gomod, gosum
  ft['lua'],       -- lua
  ft['shell'],     -- sh, bash, zsh, fish, ksh, csh, tcsh
  ft['docker'],    -- dockerfile, Dockerfile, docker-compose.yml, docker-compose.yaml
  ft['make'],      -- makefile, Makefile, make, mk, mak
  ft['markup'],    -- xml, html, jinja.html, xhtml, svg
  ft['styles'],    -- css, sass, scss, less, stylus
  ft['sql'],       -- sql, pgsql, mysql, plsql
  ft['jinja'],     -- jinja, jinja.html, sls
  ft['scpt'],      -- applescript, osascript
  ft['clj'],       -- clojure, clojurescript
  ft['php'],       -- php, php3, php4, php5, phtml
  ft['twig'],      -- twig, html.twig
  ft['stylus'],    -- stylus
  ft['smali'],     -- smali
  ft['jade'],      -- jade, pug
  ft['java'],      -- java, jsp, jspx
  ft['kotlin'],    -- kotlin, kt, kts
  ft['swift'],     -- swift
  ft['ruby'],      -- ruby, rb, erb, rake, gemspec
  ft['perl'],      -- perl, pl, pm
  ft['elixir'],    -- elixir, ex, exs
  ft['erlang'],    -- erlang, erl, hrl
  ft['scala'],     -- scala, sc
  ft['haskell'],   -- haskell, hs, lhs
  ft['zig'],       -- zig
  ft['nim'],       -- nim, nims
  ft['crystal'],   -- crystal, cr
  ft['julia'],     -- julia, jl
  ft['r'],         -- r, R, Rmd, rmd
  ft['matlab'],    -- matlab
  ft['terraform'], -- terraform, tf, tfvars, hcl
  ft['ansible'],   -- ansible, yaml.ansible
  ft['graphql'],   -- graphql, gql
  ft['proto'],     -- proto, protobuf
  ft['solidity'],  -- solidity, sol
  ft['vyper'],     -- vyper, vy
}):flatten():totable()

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

local util = require 'zfogg.util'

-- {{{ plugin list
require("lazy").setup({
  -- {{{ Do me first!
  -- impatient.nvim removed - not needed with lazy.nvim which has its own cache
  -- mapx.nvim removed - repository archived, using native vim.keymap.set instead
  -- }}}

  -- {{{ Integrate with other programs
  { 'tpope/vim-fugitive',
    cmd = { 'Git', 'G', 'Gstatus', 'Gdiff', 'Glog', 'Gblame', 'Gwrite', 'Gread' },
    lazy = true,
  },
  { 'whiteinge/diffconflicts',
    cmd = 'DiffConflicts',
    lazy = true,
  },

  { 'tmux-plugins/vim-tmux', ft = { 'tmux' } },
  { 'tmux-plugins/vim-tmux-focus-events',
    lazy = false,  -- Load immediately for tmux focus events
    enabled = not (vim.g.vscode and not vim.fn.has('nvim')),
  },

  { 'christoomey/vim-tmux-navigator',
    lazy = false,  -- Load immediately for navigation
    init = function()
      if util.inVscode or util.inKitty or (not util.inTmux) then
        vim.g.tmux_navigator_no_mappings = 1
      end
    end,
  },

  { 'knubie/vim-kitty-navigator',
    lazy = false,  -- Load immediately for kitty navigation
    enabled = not ((not util.inKitty) or util.inTmux),
  },

  { 'Olical/vim-enmasse',
    cmd = 'EnMasse',
  },
  { 'kevinhwang91/nvim-bqf',
    ft = 'qf',
    lazy = true,
  },
  -- }}}

  -- {{{ Add features and functionality.
  { 'dense-analysis/ale',
    lazy = true,
    event = { "BufReadPost", "BufNewFile" },  -- Load when opening files for linting
    config = function()
      require('rc.ale').setup()
      require('rc.ale').config()
    end,
  },


  { 'lewis6991/gitsigns.nvim',
    lazy = true,
    event = { "BufReadPost", "BufNewFile" },  -- Load when opening files to show git signs
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
    lazy = true,
    cmd = 'Telescope',
    keys = {
      '<C-t><C-t>',
      '<C-t>b',
      '<C-t>g',
      '<C-t>p',
      '<C-p>',
      '<C-t>f',
      '<C-f>',
      '<C-t>l',
      '<C-t><space>',
    },
    config = function()
      require('rc.telescope').setup()
      require('rc.telescope').config()
    end,
    dependencies = {
      'nvim-lua/popup.nvim',
      'nvim-lua/plenary.nvim',
    },
  },
  { 'nvim-telescope/telescope-frecency.nvim',
    lazy = true,
    config = function() require("telescope").load_extension("frecency") end,
    dependencies = { "kkharji/sqlite.lua", "nvim-telescope/telescope.nvim" },
  },
  { 'nvim-telescope/telescope-fzf-native.nvim',
    lazy = true,
    build = 'make',
    config = function() require("telescope").load_extension("fzf") end,
    dependencies = { "nvim-telescope/telescope.nvim" },
  },
  { 'xiyaowong/telescope-emoji.nvim',
    lazy = true,
    config = function() require("telescope").load_extension("emoji") end,
    dependencies = { "nvim-telescope/telescope.nvim" },
  },

  -- Project Management/Sessions
  { 'tpope/vim-obsession',
    cmd = 'Prosession',
  },
  { 'dhruvasagar/vim-prosession',
    lazy = true,
    cmd = { 'Prosession', 'ProsessionDelete' },
    dependencies = { 'tpope/vim-obsession' },
    init = function()
      vim.g.prosession_on_startup = 0
    end,
  },

  { 'AndrewRadev/bufferize.vim',
    cmd = 'Bufferize',
    lazy = true,
  },

  { 'dstein64/vim-startuptime',
    cmd = 'StartupTime',
    lazy = true,
    config = function() vim.g.startuptime_tries = 10 end,
  },

  { 'folke/neodev.nvim',
    lazy = true,
    ft = 'lua',
    config = function()
      require('neodev').setup({})
    end,
  },

  -- Undo tree
  { 'mbbill/undotree',
    cmd = 'UndotreeToggle',
    config = function() vim.g.undotree_SetFocusWhenToggle = 1 end,
  },

  { 'junegunn/fzf',
    build = './install --all',
    cmd = 'FZF',
    lazy = true,
    dependencies = {
      { 'junegunn/fzf.vim',
        build = ':call fzf#install()',
        cmd = { 'Files', 'Buffers', 'Rg', 'Lines', 'History', 'Commands', 'Marks', 'Windows', 'Commits', 'BLines', 'BTags', 'Tags' },
        lazy = true,
      },
      { 'yuki-ycino/fzf-preview.vim',
        cmd = { 'FzfPreviewFromResources', 'FzfPreviewBuffers', 'FzfPreviewProjectFiles' },
        lazy = true,
      },
    },
  },

  { 'voldikss/vim-floaterm',
    cmd = { 'FloatermNew', 'FloatermToggle', 'FloatermPrev', 'FloatermNext' },
    lazy = true,
  },

  -- fern
  { 'lambdalisue/fern.vim',
    lazy = false,
    cmd = 'Fern',
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
        enabled = true,
        dependencies = { 'junegunn/fzf' },
      },
    },
  },

  { 'preservim/nerdcommenter',
    lazy = true,
    keys = {
      { "<leader>c", mode = { "n", "v" } },
    },
  },

  -- sessions
  { 'xolox/vim-misc',
    lazy = true,
    event = 'VeryLazy',  -- Library functions, OK to load later
  },

  { 'lukas-reineke/lsp-format.nvim',
    event = 'LspAttach',
    lazy = true,
  },

  { 'github/copilot.vim',
    enabled = false,
    init = function() require("rc.copilot").setup() end,
    config = function() require("rc.copilot").config() end,
  },

  { 'saghen/blink.cmp',
    dependencies = { 'rafamadriz/friendly-snippets' },
    version = '1.*',
    event = 'InsertEnter',
    lazy = true,
    config = function()
      require('rc.blink').setup()
    end,
  },

  { 'jubnzv/virtual-types.nvim',
    event = 'LspAttach',
    lazy = true,
    dependencies = {
      'neovim/nvim-lspconfig',
    },
  },

  { 'j-hui/fidget.nvim',
    lazy = true,
    event = "LspAttach",
    config = function() require("fidget").setup({}) end,
  },

  { 'williamboman/mason.nvim',
    lazy = true,
    cmd = { 'Mason', 'MasonInstall', 'MasonUninstall', 'MasonUninstallAll', 'MasonLog' },
    event = { "BufReadPost", "BufNewFile" },  -- Load Mason when opening files for LSP
    config = function()
      require('rc.mason').setup()
      require('rc.mason').config()
    end,
    dependencies = {
      'williamboman/mason-lspconfig.nvim',
      'neovim/nvim-lspconfig',
      'ray-x/lsp_signature.nvim',
      'saghen/blink.cmp',
      'folke/neodev.nvim',
      'lukas-reineke/lsp-format.nvim',
    },
  },

  { 'williamboman/mason-lspconfig.nvim',
    lazy = true,
  },

  -- LSP - language server protocol
  { 'neovim/nvim-lspconfig',
    lazy = true,
    event = { "BufReadPost", "BufNewFile" },  -- Load LSP when opening files
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
    event = 'LspAttach',
    lazy = true,
    init = function() require('rc.lsp_signature').setup() end,
    config = function() require('rc.lsp_signature').config() end,
  },

  { 'weilbith/nvim-code-action-menu',
    cmd = 'CodeActionMenu',
  },

  { 'nvim-treesitter/nvim-treesitter',
    lazy = true,
    event = { "BufReadPost", "BufNewFile" },
    build = ':TSUpdate',
    config = function() require('rc.treesitter') end,
    dependencies = {
      { 'windwp/nvim-ts-autotag',
        config = function() require('nvim-ts-autotag').setup { enable = true } end,
      },
    },
  },

  { 'Shougo/context_filetype.vim',
    event = { "BufReadPost", "BufNewFile" },  -- Detect context filetype when opening files
    lazy = true,
  },
  { 'embear/vim-localvimrc',
    event = { "BufReadPost", "BufNewFile" },  -- Load local vimrc when opening files
    lazy = true,
  },

  { 'liuchengxu/vim-clap',
    lazy = true,
    cmd = 'Clap',
    build = ':call clap#installer#download_binary()',
  },
  { 'lukas-reineke/indent-blankline.nvim',
    lazy = true,
    event = { "BufReadPost", "BufNewFile" },  -- Show indent guides when opening files
    init = function() require('rc.indent-blankline').setup() end,
    config = function() require('rc.indent-blankline').config() end,
  },
  -- }}}

  -- {{{ Language support.
  { 'euclidianAce/BetterLua.vim',
    ft = 'lua',
    lazy = true,
  },
  { 'chr4/nginx.vim',
    ft = { 'nginx', 'conf' },
    lazy = true,
  },
  { 'wavded/vim-stylus', ft = ft['stylus'] },
  { 'lumiliet/vim-twig', ft = ft['twig'] },
  { 'othree/html5.vim',
    ft = { 'html', 'xhtml' },
    lazy = true,
  },
  { 'lifepillar/pgsql.vim', ft = ft['sql'] },
  { 'sheerun/vim-polyglot',
    -- Load immediately for syntax highlighting on startup
    lazy = false,
    priority = 100,  -- Load early but after colorscheme
  },

  { 'python-mode/python-mode', ft = ft['py'] },
  { 'gisphm/vim-gitignore',
    ft = 'gitignore',
    lazy = true,
  },
  { 'rust-lang/rust.vim', ft = ft['rs'] },
  { 'vim-scripts/applescript.vim', ft = ft['scpt'] },
  { 'guns/vim-clojure-highlight', ft = ft['clj'] },
  { 'leafgarland/typescript-vim',
    ft = { 'typescript', 'typescriptreact' },
    lazy = true,
  },
  { 'peitalin/vim-jsx-typescript',
    dependencies = {
      'leafgarland/typescript-vim',
    },
  },
  { 'fladson/vim-kitty',
    ft = 'kitty',
    lazy = true,
  },
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
  { 'mattn/emmet-vim',
    lazy = true,
    ft = { "html", "css", "javascript", "javascriptreact", "typescript", "typescriptreact" },
  },

  -- clang
  { 'libclang-vim/libclang-vim', ft = ft['cx'], lazy = true },

  { 'kchmck/vim-coffee-script', ft = { 'coffee' } },
  { 'tweekmonster/braceless.vim',
    ft = { 'python', 'yaml', 'haml' },
    lazy = true,
  },
  { 'chrisbra/csv.vim', ft = { 'csv' } },
  { 'cespare/vim-toml', ft = { 'toml' } },
  { 'digitaltoad/vim-pug', ft = ft['jade'] },
  { 'tpope/vim-afterimage', ft = ft['image'] },
  { 'kylef/apiblueprint.vim', ft = { 'apiblueprint' } },

  { 'PProvost/vim-ps1', ft = { 'ps1', 'ps1xml' } },
  { 'PotatoesMaster/i3-vim-syntax', ft = { 'i3' } },
  { 'darfink/vim-plist',
    ft = 'plist',
    lazy = true,
  },
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

  { 'isobit/vim-caddyfile',
    ft = 'caddyfile',
    lazy = true,
  },
  { 'wgwoods/vim-systemd-syntax',
    ft = 'systemd',
    lazy = true,
  },
  { 'vyperlang/vim-vyper',
    ft = 'vyper',
    lazy = true,
  },
  -- }}}

  -- {{{ Beautify Vim.
  { 'nvim-lualine/lualine.nvim',
    lazy = true,
    event = "VeryLazy",  -- Status line can load after initial display
    config = function() require('rc.lualine') end,
    dependencies = {
      'kyazdani42/nvim-web-devicons',
    },
  },
  { 'akinsho/bufferline.nvim',
    lazy = true,
    event = "VeryLazy",  -- Tab line can load after initial display
    config = function() require('rc.bufferline') end,
    dependencies = { 'kyazdani42/nvim-web-devicons' },
  },
  { 'chriskempson/base16-vim',
    lazy = true,
    priority = 1000,
  },
  { 'EdenEast/nightfox.nvim',
    lazy = false,
    priority = 1000,
    init = function() require('rc.nightfox').setup() end,
    config = function() require('rc.nightfox').config() end,
  },
  { 'vim-scripts/AfterColors.vim',
    event = 'ColorScheme',
    lazy = true,
  },
  { 'fedorenchik/AnsiEsc',
    cmd = 'AnsiEsc',
    lazy = true,
  },
  { 'arakashic/chromatica.nvim', ft = ft['cx'] },
  { 'markonm/traces.vim',
    event = 'CmdlineEnter',
    lazy = true,
    enabled = vim.fn.has('wsl') == 0,  -- Disabled on WSL: E716 dictionary error with nvim 0.12
  },
  { 'haya14busa/incsearch.vim',
    lazy = true,
    keys = { "/", "?", "n", "N" },
    dependencies = {
      'haya14busa/incsearch-easymotion.vim',
      'haya14busa/incsearch-fuzzy.vim',
    },
  },

  { 'haya14busa/vim-keeppad',
    event = 'VeryLazy',  -- Utility for keeping cursor position, OK to load later
    lazy = true,
  },
  -- }}}

  -- {{{ Direct text manipulation.
  { 'b4winckler/vim-angry',
    keys = { { 'gx', mode = { 'n', 'v' } } },
    lazy = true,
  },
  { 'tommcdo/vim-exchange',
    keys = { 'cx', 'cxx', 'X', 'cxc' },
    lazy = true,
  },
  { 'windwp/nvim-autopairs',
    event = 'InsertEnter',
    lazy = true,
    dependencies = {
      'nvim-treesitter/nvim-treesitter',
      'saghen/blink.cmp',
    },
    init = function() require('rc.autopairs').setup() end,
    config = function() require('rc.autopairs').config() end,
  },
  { 'tpope/vim-surround',
    keys = { 'cs', 'ds', 'ys', { 'S', mode = 'v' } },
    lazy = true,
  },

  -- textobj
  { 'kana/vim-textobj-user',
    lazy = true,
    event = { "VeryLazy", "BufReadPost", "BufNewFile" },  -- Load early so textobj plugins can use it
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
      { 'libclang-vim/vim-textobj-clang', ft = ft['cx'], enabled = true },
      { 'libclang-vim/vim-textobj-function-clang', ft = ft['cx'], enabled = true },
    },
  },

  { 'bruno-/vim-space',
    keys = { '<Space>', { '<Space>', mode = 'v' } },
    lazy = true,
  },
  { 'Konfekt/FastFold',
    event = { "BufReadPost", "BufNewFile" },  -- Setup folding when opening files
    lazy = true,
  },
  { 'Konfekt/FoldText',
    event = { "BufReadPost", "BufNewFile" },  -- Custom fold text when opening files
    lazy = true,
  },
  -- }}}

  -- {{{ Silent enhancements.
  { 'editorconfig/editorconfig-vim',
    event = 'BufReadPre',
    lazy = true,
  },
  { 'kana/vim-niceblock',
    keys = { { 'I', mode = 'v' }, { 'A', mode = 'v' } },
    lazy = true,
  },
  { 'tpope/vim-repeat',
    event = 'VeryLazy',  -- Repeat support, OK to load later
    lazy = true,
  },
  { 'vim-scripts/visualrepeat',
    keys = { { '.', mode = 'v' } },
    lazy = true,
  },
  { 'junegunn/vim-easy-align',
    cmd = 'EasyAlign',
    keys = { { 'ga', mode = { 'n', 'v' } } },
    lazy = true,
  },
  -- { 'tpope/vim-sleuth' }, -- Removed: conflicts with vim-polyglot's built-in sleuth
  { 'kana/vim-operator-user',
    lazy = true,
  },
  --{ 'haya14busa/vim-operator-flashy',
  --  keys = { 'y', 'd', 'c' },
  --  lazy = true,
  --  dependencies = { 'kana/vim-operator-user' },
  --},
  -- vim-fetch causes issues with file loading and syntax highlighting in lazy.nvim
  -- TODO: Find a better solution for file:line:column support
  -- { 'kopischke/vim-fetch',
  --   lazy = false,
  -- },
  { 'wsdjeg/vim-fetch',
    lazy = false,
  },
  { 'pbrisbin/vim-mkdir',
    event = 'BufWritePre',
    lazy = true,
  },
  { 'vim-utils/vim-vertical-move',
    keys = { '[v', ']v' },
    lazy = true,
  },

  { "monkoose/neocodeium",
    lazy = true,
    event = "InsertEnter",
    config = function()
      local neocodeium = require("neocodeium")
      neocodeium.setup({
        enabled = true,
        show_label = true,
        debounce = true,
        silent = true,
        filetypes = {
          help = false,
          gitcommit = false,
          gitrebase = false,
          ["."] = false,
        },
      })
      vim.keymap.set("i", "<Tab>", function()
        if neocodeium.visible() then
          return neocodeium.accept()
        else
          return vim.api.nvim_replace_termcodes("<Tab>", true, true, true)
        end
      end, { expr = true, silent = true })
      vim.keymap.set("i", "<C-S-n>", function()
        require("neocodeium").cycle_or_complete()
      end, { silent = true })
      vim.keymap.set("i", "<C-S-p>", function()
        require("neocodeium").cycle_or_complete(-1)
      end, { silent = true })
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
