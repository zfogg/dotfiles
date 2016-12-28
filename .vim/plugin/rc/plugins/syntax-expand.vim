"  syntax-expand
scriptencoding utf-8


func! <SID>ft_conceal_syntax(ft, hideSyntax) abort
    let l:hideChar = eval('g:'.a:ft.'_conceal_'.a:hideSyntax)
    exe 'au FileType javascript'
        \.' inoremap <silent> <buffer> '.l:hideChar
        \.' <C-r>=syntax_expand#expand('.l:hideChar.', "'.a:hideSyntax.'")<CR>'
endfunc


aug RcPlugin__syntaxExpand_conceal
    au!
    call <SID>ft_conceal_syntax('javascript', 'function')
    call <SID>ft_conceal_syntax('javascript', 'null')
    call <SID>ft_conceal_syntax('javascript', 'this')
    call <SID>ft_conceal_syntax('javascript', 'return')
    call <SID>ft_conceal_syntax('javascript', 'undefined')
    call <SID>ft_conceal_syntax('javascript', 'NaN')
    call <SID>ft_conceal_syntax('javascript', 'prototype')
    call <SID>ft_conceal_syntax('javascript', 'static')
    call <SID>ft_conceal_syntax('javascript', 'super')
    call <SID>ft_conceal_syntax('javascript', 'arrow_function')
aug END
