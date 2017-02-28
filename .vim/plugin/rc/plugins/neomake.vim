" neomake
scriptencoding utf-8


if exists('g:vimpager') | finish | endif
if !has('nvim') | finish | endif

let g:neomake_open_list                       = 0
let g:neomake_list_height                     = 5
let g:neomake_verbose                         = 0
let g:neomake_airline                         = 0
"let g:neomake_ft_maker_remove_invalid_entries = 1
let g:neomake_place_signs                     = 1


" vimscript
call neomakeft#SetupFt('vim', {
    \ 'makers':    ['vint'],
    \ 'neomakers': ['vint'],
\ })

" c-family
call neomakeft#SetupFt('c', {
    \ 'makers':    ['clang', 'clangtidy', 'clangcheck'],
    \ 'neomakers': ['clang', 'clangtidy', 'clangcheck'],
\ })
call neomakeft#SetupFt('cpp', {
\ 'makers':    ['clang', 'clangtidy', 'clangcheck'],
\ 'neomakers': ['clang', 'clangtidy', 'clangcheck'],
\ })
let s:llvm_bin = '/usr/local/opt/llvm/bin'
let g:neomake_clangtidy_exe   = s:llvm_bin.'/clang-tidy'
let g:neomake_clangcheck_exe  = s:llvm_bin.'/clang-check'
let g:neomake_clangtidy_args  = ['-p', './build']
let g:neomake_clangcheck_args = ['-p', './build']

" shell
call neomakeft#SetupFt('sh', {
    \ 'makers':    ['sh', 'shellcheck'],
    \ 'neomakers': ['sh', 'shellcheck'],
\ })
call neomakeft#SetupFt('zsh', {
    \ 'makers':    ['zsh'],
    \ 'neomakers': ['zsh'],
\ })

" js
call neomakeft#SetupFt('javascript', {
    \ 'makers':    ['eslint'],
    \ 'neomakers': ['eslint'],
\ })

" rust
call neomakeft#SetupFt('rust', {
    \ 'neomakers': ['rustc'],
\ })

" json
call neomakeft#SetupFt('json', {
    \ 'makers':    ['jsonlint'],
    \ 'neomakers': ['jsonlint'],
\ })
