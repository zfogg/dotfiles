" neosnippet
scriptencoding utf-8


if !has('nvim') | finish | endif
if !z#util#HasPlugin('neosnippet') | finish | endif


set completeopt=menuone,noinsert,noselect
let g:neosnippet#enable_completed_snippet = 1


" Plugin key-mappings.
imap <C-K> <Plug>(neosnippet_expand_or_jump)
smap <C-K> <Plug>(neosnippet_expand_or_jump)
xmap <C-K> <Plug>(neosnippet_expand_target)

" SuperTab like snippets behavior.
imap <expr><CR> <SID>RcPlug_CR()


func! <SID>RcPlug_CR() abort
    return neosnippet#expandable() ?
        \ "\<C-k>" :
        \ "\<CR>\<Plug>AutoPairsReturn"
endfunc
