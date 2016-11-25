" neomake


let g:neomake_open_list                       = 0
let g:neomake_list_height                     = 7
let g:neomake_verbose                         = 0
let g:neomake_airline                         = 0
let g:neomake_ft_maker_remove_invalid_entries = 0


" filetype

" vimscript
call neomakeft#SetupFt('vim', {
    \ 'makers': ['vint'],
\ })

" c-family
call neomakeft#SetupFt('c', {
    \ 'makers': ['clang', 'clangtidy', 'clangcheck'],
\ })
call neomakeft#SetupFt('cpp', {
    \ 'makers': ['clang', 'clangtidy', 'clangcheck'],
\ })
let g:neomake_c_clangtidy_args    = ['-p', './build']
let g:neomake_cpp_clangtidy_args  = ['-p', './build']
let g:neomake_c_clangcheck_args   = ['-p', './build']
let g:neomake_cpp_clangcheck_args = ['-p', './build']

" shell
call neomakeft#SetupFt('bash', {
    \ 'makers': ['bash', 'shellcheck'],
\ })
call neomakeft#SetupFt('zsh', {
    \ 'makers': ['zsh'],
\ })

" js
call neomakeft#SetupFt('javascript', {
    \ 'makers': ['jshint'],
\ })
call neomakeft#SetupFt('jsx', {
    \ 'makers': ['jshint'],
\ })
