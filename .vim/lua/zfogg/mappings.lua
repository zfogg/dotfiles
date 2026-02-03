-- lua/zfogg/mappings.lua

-- Leader keys
vim.g.mapleader = ','
vim.g.maplocalleader = ','

-- Mode aliases for readability
local n = 'n'
local i = 'i'
local v = 'v'
local x = 'x'
local c = 'c'
local o = 'o'

-- Helper for setting keymaps
local function map(modes, lhs, rhs, opts)
  opts = opts or {}
  vim.keymap.set(modes, lhs, rhs, opts)
end

-- Escape key alternative
map(i, '<C-c>', '<Esc>')

-- Use semicolon for command mode
map('n', ';', ':')
-- map('n', ';;', ';')  -- uncomment to map back to semicolon

-- Don't jump cursor when using * to search for word under cursor
map('n', '*', '*``')
map('n', '#', '#``')

-- Reselect visual block after indent/outdent
map(x, '<', '<gv')
map(x, '>', '>gv')

-- Opposite of J. Split line at current point.
map('n', '<A-S-j>', 'a<CR><Esc>k$')

-- Keep cursor in centre of screen after motions
map('n', '<C-d>', '<C-d>zz')
map('n', '<C-u>', '<C-u>zz')
map('n', '{', '{zz')
map('n', '}', '}zz')

-- Write commands
map('n', '<Leader>w', ':w<CR>')
map('n', '<Leader>W', ':w<CR>')
map(i, '<Leader>W', '<Esc>:w<CR>a')
map('c', 'w!', 'w !sudo tee % >/dev/null')

-- Quit
map('n', '<Leader>q', ':q<CR>')

-- Vimrc editing
if vim.g.myvimrc_f and vim.fn.filereadable(vim.g.myvimrc_f) == 1 then
  local vimdir = vim.fn.resolve(vim.fn.fnamemodify(vim.g.myvimrc, ':p:h'))
  vim.api.nvim_create_user_command('EditVimrc', function()
    vim.cmd('silent! $tabe! ' .. vim.g.myvimrc_f)
    vim.cmd('Fern ' .. vimdir .. ' -drawer -reveal=% -wait')
    vim.cmd('wincmd p')
  end, {})

  vim.api.nvim_create_user_command('ReloadVimrc', function()
    vim.cmd('so ' .. vim.g.myvimrc_f)
    vim.cmd('redraw!')
    vim.notify("⚠️  vimrc reloaded ‼️", vim.log.levels.INFO)
  end, {})

  map('n', '<Leader>v', ':EditVimrc<CR>', { silent = true })
  map('n', '<Leader>V', ':ReloadVimrc<CR>', { silent = true })
end

-- Disabled default commands
map('n', 'Q', '<Nop>')
map({ 'n', 'x', 'o' }, '<Up>', '<Nop>')
map({ 'n', 'x', 'o' }, '<Down>', '<Nop>')
map({ 'n', 'x', 'o' }, '<Left>', '<Nop>')
map({ 'n', 'x', 'o' }, '<Right>', '<Nop>')
map(i, '<Up>', '<Nop>')
map(i, '<Down>', '<Nop>')
map(i, '<Left>', '<Nop>')
map(i, '<Right>', '<Nop>')

-- Yanking and pasting
map({ 'n', 'x', 'o' }, 'Y', 'y$')
map({ 'n', 'x', 'o' }, '<Leader>y', '"+y')
map('n', '<Leader>y%', ':let @*=expand("%")<Bar>echo getreg("*")<CR>')
map('n', '<Leader>Y%', ':let @*=expand("%:p")<Bar>echo getreg("*")<CR>')
map({ 'n', 'x', 'o' }, '<Leader>p', '"+p')
map('x', 'p', 'v:register==\'"\'?\'pgvy\':\'p\'', { expr = true })

-- Cursor motion - VSCode handling
if vim.g.vscode then
  map('n', '<C-h>', '<Cmd>call VSCodeNotify("workbench.action.focusLeftGroup")<CR>')
  map('n', '<C-j>', '<Cmd>call VSCodeNotify("workbench.action.focusBelowGroup")<CR>')
  map('n', '<C-k>', '<Cmd>call VSCodeNotify("workbench.action.focusAboveGroup")<CR>')
  map('n', '<C-l>', '<Cmd>call VSCodeNotify("workbench.action.focusRightGroup")<CR>')
else
  -- j and k work on long wrapped lines
  map({ 'n', 'x', 'o' }, 'j', 'gj')
  map({ 'n', 'x', 'o' }, 'k', 'gk')
end

-- Scroll through items in the locations list
map('n', 'mm', ':lprevious<CR>')

-- Scrolling effect
map({ 'n', 'x', 'o' }, '<a-j>', '<c-e>j')
map({ 'n', 'x', 'o' }, '<a-k>', '<c-y>k')
map({ 'n', 'x', 'o' }, '<a-l>', 'zll')
map({ 'n', 'x', 'o' }, '<a-h>', 'zhh')

-- Join lines mappings
map('n', '<Leader>oO', 'Dk$p<S-v>=')
map(i, '<Leader>oO', '<C-o>D<C-o>k<C-o>$<C-o>p')
map('n', '<Leader>OO', 'Dk$P<S-v>=')
map(i, '<Leader>OO', '<C-o>D<C-o>k<C-o>$<C-o>P')

-- Join next line to this one
map(i, 'JJ', '<C-o>J<C-o>==')

-- Join this line to previous
map(i, 'KK', '<C-o>k<C-o>J<C-o>==')

-- Undo changes since InsertEnter
map(i, 'uu', '<C-o>u')

-- Quick navigation in insert/command mode
map(i, 'jj', '<Esc>')
map(i, 'kk', '<Esc>:w<CR>')
map(c, 'kk', '<Up>')
map(c, 'jj', '<Down>')

map(i, 'hH', '<Home>')
map(i, 'lL', '<End>')
map(c, 'hH', '<Home>')
map(c, 'lL', '<End>')

map({ 'n', 'x', 'o' }, '<PageUp>', '<S-h>zz<S-l>')
map(i, '<PageUp>', '<C-o>zt')
map('n', '<PageDown>', '<S-l>zz<S-h>')
map(i, '<PageDown>', '<C-o>zb')

map({ 'n', 'x', 'o', 'i', 'c' }, '<M-Right>', '<M-S-Right>')
map(i, '<M-Right>', '<M-S-Right>')
map(i, '<M-Left>', '<M-S-Left>')
map({ 'n', 'x', 'o', 'i', 'c' }, '<M-Left>', '<M-S-Left>')

-- Folds
map({ 'n', 'x' }, '<Space>', 'za')

-- Tabs and splits
if vim.g.vscode then
  map('n', '<Leader><C-t>', '<Cmd>call VSCodeNotify("workbench.action.newWindowTab")<CR>')
  map('n', '<Leader>x', '<Cmd>call VSCodeNotify("keyboard-quickfix.openQuickFix")<CR>')
  map('n', '<Leader>j', '<Cmd>call VSCodeNotify("workbench.action.quickOpenPreviousRecentlyUsedEditorInGroup")<CR>')
  map('n', '<Leader>k', '<Cmd>call VSCodeNotify("workbench.action.quickOpenLeastRecentlyUsedEditorInGroup")<CR>')
else
  map('n', '<Leader><C-t>', ':tabnew<CR>')
  map('n', '<Leader>x', ':tabclose<CR>')
  map('n', '<Leader>j', ':tabprevious<CR>')
  map('n', '<Leader>k', ':tabnext<CR>')

  -- Resize splits
  map('n', '<S-Up>', '5<C-W>+')
  map('n', '<S-Down>', '5<C-W>-')
  map('n', '<S-Right>', '5<C-W>>')
  map('n', '<S-Left>', '5<C-W><')
  map('n', '<Up>', '<C-W>+')
  map('n', '<Down>', '<C-W>-')
  map('n', '<Right>', '<C-W>>')
  map('n', '<Left>', '<C-W><')
end

-- Miscellaneous
map('x', 'R', 'r<Space>R')

-- Delete trailing whitespace
-- map('n', '<Leader>S', ':call z#util#TrimWhitespace()<CR>')

-- Fixes for display glitches
map('n', '<Leader><Space>', ':nohlsearch<C-R>=' .. (vim.fn.has('diff') == 1 and '"\\|diffupdate"' or '""') .. '<CR><CR>')
map('n', '<Leader>rd', ':redraw!<CR>')

-- Change to the directory of the current buffer's file
map('n', '<Leader>cd', ':cd  %:p:h<BAR>pwd<CR>')

-- Incremental command preview
if vim.opt.inccommand:get() then
  -- Already set
end

-- VSCode specific
if vim.g.vscode then
  map('n', 'z=', '<Cmd>call VSCodeNotify("keyboard-quickfix.openQuickFix")<CR>')
end

-- Save shortcuts from various modes
map({ "i", "x", "n", "s" }, "<D-s>", "<cmd>w<CR>", { desc = "Save file" })
map({ "i", "x", "n", "s" }, "<C-s>", "<cmd>w<CR>", { desc = "Save file" })

-- Window navigation with Ctrl-hjkl (fallback when tmux/kitty navigator disabled)
-- Only set these if vim-kitty-navigator and vim-tmux-navigator are not handling them
local inTmux = string.len(vim.env.TMUX or '') > 0
local inKitty = vim.env.KITTY_WINDOW_ID ~= nil
if not (inKitty or inTmux) then
  vim.keymap.set('n', '<C-h>', '<C-w>h', { desc = 'Move to left window', silent = true })
  vim.keymap.set('n', '<C-j>', '<C-w>j', { desc = 'Move to window below', silent = true })
  vim.keymap.set('n', '<C-k>', '<C-w>k', { desc = 'Move to window above', silent = true })
  vim.keymap.set('n', '<C-l>', '<C-w>l', { desc = 'Move to right window', silent = true })
end
