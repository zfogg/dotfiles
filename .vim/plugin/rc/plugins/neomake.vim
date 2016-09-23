" neomake


let g:neomake_open_list                       = 0
let g:neomake_list_height                     = 7
let g:neomake_verbose                         = 0
let g:neomake_airline                         = 0
let g:neomake_ft_maker_remove_invalid_entries = 0


" neomakeft

" meta
call neomakeft#SetupFt('vim', {
    \ 'makers': ['vint'],
\ })

" c-family
call neomakeft#SetupFt('c', {
    \ 'makers': ['clang', 'clangcheck'],
\ })
call neomakeft#SetupFt('cpp', {
    \ 'makers': ['clang', 'clangcheck'],
\ })

" shell
call neomakeft#SetupFt('bash', {
    \ 'makers': ['bash', 'shellcheck'],
\ })
call neomakeft#SetupFt('zsh', {
    \ 'makers': ['zsh'],
\ })
