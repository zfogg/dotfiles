" plugin/rc/auto-pairs
scriptencoding utf-8

if !has('nvim') && !PHas('auto-pairs') | finish | endif

" NOTE: for neomake / deoplete / neosnippet
"   https://github.com/jiangmiao/auto-pairs/issues/91
let g:AutoPairsMapCR=0
" place the following last in any `imap <expr><CR>` mapping
"   <Plug>AutoPairsReturn
