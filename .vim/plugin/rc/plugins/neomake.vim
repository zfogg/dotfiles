" neomake
scriptencoding utf-8


if exists('g:vimpager') | finish | endif
if !has('nvim') | finish | endif


let g:neomake_open_list                       = 0
let g:neomake_list_height                     = 7
let g:neomake_verbose                         = 0
let g:neomake_airline                         = 0
let g:neomake_ft_maker_remove_invalid_entries = 0
let g:neomake_place_signs                     = 1


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
    \ 'makers'    : ['bash', 'shellcheck'],
\ })
call neomakeft#SetupFt('zsh', {
    \ 'makers'    : ['zsh'],
    \ 'neomakers' : ['zsh'],
\ })

" js
call neomakeft#SetupFt('javascript', {
    \ 'makers'    : ['jshint', 'eslint'],
    \ 'neomakers' : ['jshint', 'eslint'],
\ })
call neomakeft#SetupFt('jsx', {
    \ 'makers'    : ['jshint', 'eslint'],
    \ 'neomakers' : ['jshint', 'eslint'],
\ })

" json
call neomakeft#SetupFt('json', {
    \ 'makers'    : ['jsonlint'],
    \ 'neomakers' : ['jsonlint'],
\ })
