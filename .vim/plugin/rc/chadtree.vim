" plugin/rc/nerdtree
scriptencoding utf-8


if !PHas('chadtree') | finish | endif
if exists('g:vimpager') | finish | endif


nnoremap <Leader>n<Space> <CMD>CHADopen<CR>
