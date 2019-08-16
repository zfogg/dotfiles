" context_filetype
scriptencoding utf-8


if !exists('g:context_filetype#filetypes')
    let g:context_filetype#filetypes = {}
endif


if !exists('g:context_filetype#same_filetypes')
    let g:context_filetype#same_filetypes = {}
endif
" In c buffers, completes from cpp and d buffers.
let g:context_filetype#same_filetypes.c = 'cpp,d'
" In cpp buffers, completes from c buffers.
let g:context_filetype#same_filetypes.cpp = 'c'
" In gitconfig buffers, completes from all buffers.
let g:context_filetype#same_filetypes.gitconfig = '_'
" In default, completes from all buffers.
let g:context_filetype#same_filetypes._ = '_'
