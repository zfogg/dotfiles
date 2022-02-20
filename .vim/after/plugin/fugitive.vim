"  fugitive
scriptencoding utf-8


if !PHas('vim-fugitive') | finish | endif

let g:fugitive_pty = 0
nmap <Leader>gc :Gcommit<CR>
nmap <Leader>gd :Gdiff<CR>
nmap <Leader>gl :Glog<CR>
nmap <Leader>gs :Gstatus<CR>
nmap <Leader>gR :Gread<CR>
nmap <Leader>gW :Gwrite<CR>:e<CR>
nmap <Leader>gp :Git push<CR>
nmap <Leader>gb :Gblame<CR>
nmap <Leader>gH :Gbrowse<CR>
