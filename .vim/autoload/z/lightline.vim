" autoload/z/lightline
scriptencoding utf-8


function! BufferIsPlugin() abort
    let l:fname = expand('%:t')
    let l:pnames = [
        \ '__Tag_List__'
        \,'ControlP'
        \,'NERD_tree_'
    \ ]
    for l:pname in l:pnames
        if l:fname =~? l:pname
            return v:true
        endif
    endfor
    return v:false
endfunction

function! BufferIsWide() abort
  return winwidth(0) > 70
endfunction

function! LightlineVisible() abort
  return !BufferIsPlugin() && BufferIsWide()
endfunction


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
    if LightlineVisible() && exists('*fugitive#head')
        let l:branch = fugitive#head()
        return l:branch !=# '' ? ' ' . l:branch : ''
    else
        return ''
    endif
endfunc


func! z#lightline#Filename() abort
    if LightlineVisible()
        let l:fname = expand('%:t')
        return ('' !=? z#lightline#Readonly() ?       z#lightline#Readonly() . ' ' : '') .
            \  ('' !=# l:fname ? expand('%:h:t').'/'.l:fname : '[No name]')              .
            \  ('' !=? z#lightline#Modified() ? ' ' . z#lightline#Modified()       : '')
    else
        return ''
    endif
endfunc


func! z#lightline#Fileformat() abort
    return LightlineVisible() ? &fileformat : ''
endfunc

func! z#lightline#Lineinfo() abort
    return !BufferIsPlugin() ? '%3l:%-2v' : ''
endfunc


function! z#lightline#BytePercent() abort
    let l:byte = line2byte(line( '.' )    ) + col( '.' ) - 1
    let l:size = line2byte(line( '$' ) + 1)              - 1
    return (l:byte * 100) / l:size
endfunction

func! z#lightline#Percent() abort
    return LightlineVisible()
        \ ? (z#lightline#BytePercent() . '%')
        \ : ''
endfunc


func! z#lightline#Filetype() abort
    return LightlineVisible()
        \ ? (strlen(&filetype) ? &filetype : '[No ft]')
        \ : ''
endfunc


func! z#lightline#Fileencoding() abort
    return LightlineVisible()
        \ ? (strlen(&fenc) ? &fenc : &enc)
        \ : ''
endfunc


func! z#lightline#Mode() abort
    return BufferIsPlugin()
        \ ? '[Plugin]'
        \ : BufferIsWide()
            \ ? lightline#mode()
            \ : ''
endfunc


func! z#lightline#Neomake() abort
    return (LightlineVisible() && has('nvim'))
        \ ? '%{neomake#statusline#LoclistStatus()}'
        \ : ''
endfunc


func! z#lightline#OnNeomakeCountsChanged() abort
    call lightline#update()
endfunc
