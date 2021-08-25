" plugin/rc/vim-which-key
scriptencoding utf-8


if !PHas('vim-which-key') | finish | endif


let g:which_key_map = get(g:, 'which_key_map', {
    \ 'w': {
        \ 'name' : '+windows' ,
        \ 'w' : ['<C-W>w'     , 'other-window']          ,
        \ 'd' : ['<C-W>c'     , 'delete-window']         ,
        \ '-' : ['<C-W>s'     , 'split-window-below']    ,
        \ '|' : ['<C-W>v'     , 'split-window-right']    ,
        \ '2' : ['<C-W>v'     , 'layout-double-columns'] ,
        \ 'h' : ['<C-W>h'     , 'window-left']           ,
        \ 'j' : ['<C-W>j'     , 'window-below']          ,
        \ 'l' : ['<C-W>l'     , 'window-right']          ,
        \ 'k' : ['<C-W>k'     , 'window-up']             ,
        \ 'H' : ['<C-W>5<'    , 'expand-window-left']    ,
        \ 'J' : [':resize +5'  , 'expand-window-below']   ,
        \ 'L' : ['<C-W>5>'    , 'expand-window-right']   ,
        \ 'K' : [':resize -5'  , 'expand-window-up']      ,
        \ '=' : ['<C-W>='     , 'balance-window']        ,
        \ 's' : ['<C-W>s'     , 'split-window-below']    ,
        \ 'v' : ['<C-W>v'     , 'split-window-below']    ,
        \ '?' : ['Windows'    , 'fzf-window']            ,
    \ },
    \ 'b': {
        \ 'name' : '+buffer' ,
        \ '1' : ['b1'        , 'buffer 1']        ,
        \ '2' : ['b2'        , 'buffer 2']        ,
        \ 'd' : ['bd'        , 'delete-buffer']   ,
        \ 'f' : ['bfirst'    , 'first-buffer']    ,
        \ 'h' : ['Startify'  , 'home-buffer']     ,
        \ 'l' : ['blast'     , 'last-buffer']     ,
        \ 'n' : ['bnext'     , 'next-buffer']     ,
        \ 'p' : ['bprevious' , 'previous-buffer'] ,
        \ '?' : ['Buffers'   , 'fzf-buffer']      ,
    \ },
    \ 'l': {
        \ 'name' : '+lsp',
        \ 'f' : ['spacevim#lang#util#Format()'          , 'formatting']       ,
        \ 'r' : ['spacevim#lang#util#FindReferences()'  , 'references']       ,
        \ 'R' : ['spacevim#lang#util#Rename()'          , 'rename']           ,
        \ 's' : ['spacevim#lang#util#DocumentSymbol()'  , 'document-symbol']  ,
        \ 'S' : ['spacevim#lang#util#WorkspaceSymbol()' , 'workspace-symbol'] ,
        \ 'g' : {
            \ 'name': '+goto',
            \ 'd' : ['spacevim#lang#util#Definition()'     , 'definition']      ,
            \ 't' : ['spacevim#lang#util#TypeDefinition()' , 'type-definition'] ,
            \ 'i' : ['spacevim#lang#util#Implementation()' , 'implementation']  ,
            \ },
        \ },
    \ })

aug RcSettings_vim_which_key
    au!
    au User vim-which-key call which_key#register(',', 'g:which_key_map')
    au FileType which_key
    au FileType which_key       set laststatus=0 noshowmode noruler
        \| au BufLeave <buffer> set laststatus=2   showmode   ruler
augroup END

"call which_key#register('<Space>', 'g:which_key_map')
exec 'nnoremap <silent> <Leader> :<c-u>WhichKey '      ."','".'<CR>'
exec 'vnoremap <silent> <Leader> :<c-u>WhichKeyVisual '."','".'<CR>'


"exec 'nnoremap <silent> <Leader> :WhichKey '.g:mapleader.'<CR>'
