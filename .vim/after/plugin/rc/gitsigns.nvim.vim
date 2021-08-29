" plugin/rc/gitsigns.nvim
scriptencoding utf-8


if !PHas('gitsigns.nvim') | finish | endif


if get(b:, 'rcplugin_gitsigns', 0) == 1 | finish | endif
let b:rcplugin_gitsigns = 1


lua << EOF
require('gitsigns').setup()
EOF

"set statusline+=%{get(b:,'gitsigns_status','')}
"set statusline+=%{get(b:,'gitsigns_head','')}
