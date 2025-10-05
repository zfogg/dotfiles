" after/plugin/fern.vim
" Keymaps for Fern - these load immediately, but Fern itself is lazy-loaded

nnoremap <silent> <Leader>n<Space> :Fern . -drawer -toggle<CR>
nnoremap <silent> <Leader>nn :Fern . -drawer -wait -reveal=%<BAR>wincmd p<CR>