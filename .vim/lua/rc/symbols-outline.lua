-- lua/rc/symbols-outline.lua
local m = require('rc.mapx')

m.nname('<Leader>m', 'symbols & tags')
nnoremap('<Leader>m<Space>', ':SymbolsOutline<CR>',      'silent')
nnoremap('<Leader>mm',       ':SymbolsOutlineOpen<CR>',  'silent')
nnoremap('<Leader>mq',       ':SymbolsOutlineClose<CR>', 'silent')
