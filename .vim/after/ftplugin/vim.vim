" after/ftplugin/vim
scriptencoding utf-8

" Do i even need expandtab?
setl shiftwidth=2 softtabstop=2 tabstop=2

" Use :help for K in vimscript
setl keywordprg=:help

" Use curly-braces to fold in vimscript
setl foldmethod=marker

setl iskeyword -=
setl iskeyword +=:
setl iskeyword +=-

" Execute the current line
nnoremap <buffer> <Leader><LocalLeader>el :execute getline('.')<CR>
" Execute file
nnoremap <buffer> <Leader><LocalLeader>ef :source %<CR>

" FIXME
"execute(z#util#Shiftwidth())
"echom z#util#Shiftwidth()
if exists('*shiftwidth')
    func s:sw()
        return shiftwidth()
    endfunc
else
    func s:sw()
        return &sw
    endfunc
endif


if ! exists('b:undo_ftplugin') || ! exists('g:vim_indent_cont')
    " INFO: 'ft-vim-indent'
    let g:vim_indent_cont = 2 * s:sw() " only expand to shiftwidth when <CR>\
endif

let b:undo_ftplugin = ftplugin#undo({
    \ 'opts': [
    \     'shiftwidth',
    \     'tabstop',
    \     'softtabstop',
    \     'keywordprg',
    \     'foldmethod',
    \     'iskeyword',
    \ ],
    \ 'vars': [
    \ ],
    \ 'commands': [
    \ ],
    \ 'maps': [
    \   [ 'n', '<Leader><LocalLeader>el', ],
    \   [ 'n', '<Leader><LocalLeader>ef', ],
    \ ],
    \ 'funcs': [
    \     's:sw',
    \ ],
    \ 'custom': [
    \ ],
    \ })
