"  plugin/rc/vim-syntax-expand
scriptencoding utf-8


if z#util#HasPlugin('vim-syntax-expand')
    aug RcPlugin__vimSyntaxExpand
        au!
        au FileType javascript inoremap <silent> <buffer> @ <C-r>=syntax_expand#expand("@", "this")<CR>
        au FileType javascript inoremap <silent> <buffer> < <C-r>=syntax_expand#expand_head("<", "return")<CR>
    aug END
endif
