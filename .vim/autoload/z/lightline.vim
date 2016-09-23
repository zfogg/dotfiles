" autoload/z/lightline
scriptencoding utf-8


func! z#lightline#Readonly() abort
    if &ft ==? 'help' | return ''
    elseif &readonly  | return '🔒'
    else              | return '' | endif
endfunc


func! z#lightline#Modified() abort
    if &ft ==? 'help'   | return ''
    elseif &modified    | return '+'
    elseif &modifiable  | return '-'
    else                | return '' | endif
endfunc


func! z#lightline#Fugitive() abort
    try
        if expand('%:t') !~? 'NERD' && &ft !~? 'vimfiler' && exists('*fugitive#head')
        let l:branch = fugitive#head()
        return l:branch !=# '' ? ' ' . l:branch : ''
        endif
    catch
    endtry
    return ''
endfunc


func! z#lightline#Filename() abort
    let l:fname = expand('%:t')
    return ('' !=? z#lightline#Readonly() ?       z#lightline#Readonly() . ' ' : '') .
        \  ('' !=# l:fname ? expand('%:h:t').'/'.l:fname : '[No Name]') .
        \  ('' !=? z#lightline#Modified() ? ' ' . z#lightline#Modified()       : '')
endfunc


func! z#lightline#Fileformat() abort
    return winwidth(0) > 70 ? &fileformat : ''
endfunc


func! z#lightline#Filetype() abort
    return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype : 'no ft') : ''
endfunc


func! z#lightline#Fileencoding() abort
    return winwidth(0) > 70 ? (strlen(&fenc) ? &fenc : &enc) : ''
endfunc


func! z#lightline#Mode() abort
    let l:fname = expand('%:t')
    return l:fname =~? 'NERD_tree' ? 'NERDTree' :
        \ winwidth(0) > 60 ? lightline#mode() : ''
endfunc


func! z#lightline#Neomake() abort
    if has('nvim')
        return '%{neomake#statusline#LoclistStatus()}'
    endif
    return ''
endfunc


func! z#lightline#OnNeomakeCountsChanged() abort
    call lightline#update()
endfunc
