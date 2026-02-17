-- lua/zfogg/settings.lua

-- Add treesitter parser directory to runtimepath
vim.opt.runtimepath:append(vim.fn.stdpath('data') .. '/site')

-- INFO: :help g:do_filetype_lua
vim.g.do_filetype_lua    = 1
vim.g.did_load_filetypes = 0

-- Suppress prompts that can hang without TTY
vim.opt.shortmess:append("I")

-- Line numbers
vim.opt.number = true
vim.opt.numberwidth = 2

-- General settings
vim.opt.title = true
vim.opt.showtabline = 1
vim.opt.history = 2048
vim.opt.errorbells = false
vim.opt.visualbell = true
vim.opt.updatetime = 220
vim.g.netrw_dirhistmax = 0

-- Ruby host for neovim
if vim.fn.has('nvim') == 1 then
  vim.g.ruby_host_prog = vim.fn.expand('~/.gem/bin/neovim-ruby-host')
end

-- Session and view options
vim.opt.sessionoptions:remove({ 'blank', 'buffers', 'localoptions', 'help' })
vim.opt.sessionoptions:append({ 'curdir', 'globals', 'options', 'tabpages', 'folds', 'resize', 'winpos', 'winsize' })
vim.opt.viewoptions:remove({ 'options', 'localoptions' })
vim.opt.viewoptions:append({ 'cursor', 'curdir', 'folds' })

-- Path and cdpath for file searching
vim.opt.path:append({ '.', '', '**' })
if vim.env.CDPATH then
  local cdpath_escaped = vim.env.CDPATH:gsub('[, ]', '\\%0'):gsub(':', ',')
  vim.opt.cdpath = ',' .. cdpath_escaped .. ',,,**'
else
  vim.opt.cdpath:append({ '.', '', '**' })
end

-- Wild menu and completion
vim.opt.wildmenu = true
vim.opt.wildignorecase = true
vim.opt.wildchar = vim.fn.char2nr("\t")  -- Tab character
vim.opt.wildmode = { 'longest:full', 'full' }
vim.opt.wildignore:append({
  '*.o', '*.obj', '*.so', '*.exe', '*.dll', '*.manifest', '*.dmg',
  '*.swp', '*.pyc', '*.class',
  '*.tar', '*.bz', '*.gz', '*.xz', '*.zip', '*.7z', '*.rar',
  '*/.git/*', '*/node_modules/*',
  '*.swc', '*.swc.old',
  '*.DS_Store',
  '*~', '~*'
})

if vim.fn.has('nvim') == 1 then
  vim.opt.wildoptions = 'pum'
  vim.opt.pumheight = 25
  vim.opt.pumwidth = 48
  vim.opt.pumblend = 32
end

vim.opt.complete:append('i')
vim.opt.complete:remove('t')
vim.opt.completeopt = { 'menu', 'menuone', 'preview' }
vim.opt.conceallevel = 2
vim.opt.concealcursor = 'nvic'

-- Dictionary
local unix_dictionary = '/usr/share/dict/words'
if vim.fn.has('unix') == 1 and vim.fn.filereadable(unix_dictionary) == 1 then
  vim.opt.dictionary = unix_dictionary
end

-- Undo, swap, backup
if vim.fn.has('persistent_undo') == 1 then
  vim.opt.undofile = true
end
vim.opt.swapfile = true
vim.opt.backupdir:remove('.')
vim.opt.directory:remove('.')

vim.g.omni_sql_no_default_maps = 1

-- Moving around and editing
vim.opt.startofline = false
vim.opt.virtualedit = 'all'
vim.opt.backspace = { 'indent', 'eol', 'start' }
vim.opt.autoindent = true
vim.opt.redrawtime = 1200
vim.opt.maxmempattern = 100000
vim.opt.ttimeoutlen = 100

-- cpoptions
vim.opt.cpo:append('aA')
vim.opt.cpo:remove('b')
vim.opt.cpo:append('B')
vim.opt.cpo:append('c')
vim.opt.cpo:remove('C')
vim.opt.cpo:append('d')
vim.opt.cpo:remove('D')
vim.opt.cpo:append('Ee')
vim.opt.cpo:append('fF')
vim.opt.cpo:remove('i')
vim.opt.cpo:remove('J')
vim.opt.cpo:remove('K')
vim.opt.cpo:remove('lL')
vim.opt.cpo:remove('m')
vim.opt.cpo:append('M')
vim.opt.cpo:append('n')
vim.opt.cpo:append('o')
vim.opt.cpo:remove('O')
vim.opt.cpo:remove('p')
vim.opt.cpo:append('P')
vim.opt.cpo:append('q')
vim.opt.cpo:remove('r')
vim.opt.cpo:append('R')
vim.opt.cpo:append('s')
vim.opt.cpo:remove('S')
vim.opt.cpo:append('t')
vim.opt.cpo:remove('u')
vim.opt.cpo:remove('v')
vim.opt.cpo:append('W')
vim.opt.cpo:remove('x')
vim.opt.cpo:append('X')
vim.opt.cpo:remove('y')
vim.opt.cpo:append('Z')
vim.opt.cpo:remove('!')
vim.opt.cpo:remove('$')
vim.opt.cpo:remove('%')
vim.opt.cpo:append('+')
vim.opt.cpo:remove('>')
vim.opt.cpo:append(';')
if vim.fn.has('nvim') == 1 then
  vim.opt.cpo:append('_')
end

-- formatoptions
vim.opt.formatoptions:append('c')
vim.opt.formatoptions:append('j')
vim.opt.formatoptions:append('l')
vim.opt.formatoptions:append('n')
vim.opt.formatoptions:append('q')
vim.opt.formatoptions:append('r')
vim.opt.formatoptions:append('w')
vim.opt.formatoptions:remove('t')
vim.opt.formatoptions:append('o')

-- Display aesthetics
vim.opt.cursorline = true
vim.opt.cursorcolumn = false
vim.opt.ruler = true
vim.opt.scrolljump = 4
vim.opt.scrolloff = 4
vim.opt.sidescroll = 1
vim.opt.sidescrolloff = 8
vim.opt.showmatch = true
vim.opt.list = true
vim.opt.listchars = {
  eol = '¬',
  tab = '→ ',
  trail = '·',
  extends = '⟩',
  precedes = '⟨',
  nbsp = '␣'
}
vim.opt.fillchars = {
  vert = ' ',
  stl = ' ',
  stlnc = ' '
}

-- Whitespace
vim.opt.expandtab = true
vim.opt.copyindent = true
vim.opt.preserveindent = true
vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 0
vim.opt.shiftround = true
vim.opt.wrap = false
vim.opt.linebreak = true
vim.opt.wrapmargin = 0
vim.opt.textwidth = 0
vim.opt.showbreak = '↪\\'

-- Folding
vim.opt.foldenable = true
vim.opt.foldmethod = 'syntax'
vim.opt.foldlevelstart = 99  -- Start with all folds open
vim.opt.foldopen:append({ 'percent', 'quickfix', 'tag', 'undo' })
vim.opt.foldnestmax = 3
vim.opt.foldminlines = 3
vim.g.fastfold_skip_filetypes = {
  'taglist', 'fern', 'nerdtree', 'help', 'vim'
}

-- Reading and writing
vim.opt.autowrite = false
vim.opt.autowriteall = false
vim.opt.autoread = false
vim.opt.modeline = true
vim.opt.modelines = 3
vim.opt.fileformats = { 'unix', 'mac', 'dos' }

-- shortmess
vim.opt.shortmess:append('a')
vim.opt.shortmess:remove('O')
vim.opt.shortmess:append('O')
vim.opt.shortmess:append('s')
vim.opt.shortmess:append('t')
vim.opt.shortmess:append('T')
vim.opt.shortmess:remove('W')
vim.opt.shortmess:remove('A')
vim.opt.shortmess:append('I')
vim.opt.shortmess:append('c')
vim.opt.shortmess:remove('q')
vim.opt.shortmess:remove('F')

-- Messages, info, and statuses
vim.opt.confirm = true
vim.opt.showcmd = true
vim.opt.showmode = false
vim.opt.report = 0
vim.opt.laststatus = 2

-- Searching and Patterns
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.smarttab = true
vim.opt.smartindent = true
vim.opt.cindent = true
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.regexpengine = 1

-- Set grep program
if vim.fn.executable('rg') == 1 then
  vim.opt.grepprg = '/usr/bin/env rg -H --vimgrep --context=0'
  vim.opt.grepformat = '%f:%l:%c:%m'
else
  vim.opt.grepprg = 'grep --color=never -e --exclude-dir .git -nrI $* . /dev/null'
end

-- Character classes
-- isfname is complex and varies by platform; usually has good defaults
vim.opt.isident:append('@,48-57,_,192-255')
vim.opt.iskeyword:append('@,48-57,_,192-255')

-- Syntax
vim.cmd('syntax sync minlines=16 maxlines=512 linebreaks=1')
vim.opt.synmaxcol = 700

-- Treesitter highlighting (built-in, just needs parsers installed via :TSInstall)
vim.api.nvim_create_autocmd("FileType", {
  group = vim.api.nvim_create_augroup("TreesitterHighlight", { clear = true }),
  callback = function(args)
    pcall(vim.treesitter.start, args.buf)
  end,
})

-- Clipboard
if vim.fn.has('osx') == 1 then
  vim.g.clipboard = {
    name = 'pbcopy',
    copy = { ['+'] = 'pbcopy', ['*'] = 'pbcopy' },
    paste = { ['+'] = 'pbpaste', ['*'] = 'pbpaste' },
    cache_enabled = 1,
  }
elseif vim.fn.has('unix') == 1 then
  vim.g.clipboard = {
    name = 'xclip',
    copy = { ['+'] = 'xclip -selection clipboard -in', ['*'] = 'xclip -selection clipboard -in' },
    paste = { ['+'] = 'xclip -selection clipboard -out', ['*'] = 'xclip -selection clipboard -out' },
    cache_enabled = 1,
  }
end

-- Mouse
if vim.fn.has('mouse_sgr') == 1 then
  vim.opt.ttymouse = 'sgr'
end

-- Kitty terminal autocommand
local kitty_augroup = vim.api.nvim_create_augroup("kitty_conf", { clear = true })
vim.api.nvim_create_autocmd("BufWritePost", {
  pattern = "*/.config/kitty/*.conf",
  group = kitty_augroup,
  callback = function()
    vim.cmd [[
      silent !kitty @ load-config
      redraw!
      echom "Reloaded kitty config"
    ]]
  end,
})
