" autoload/neomake/ft/rust
scriptencoding utf-8


function! neomake#makers#ft#rust#EnabledMakers() abort
    return ['rustc']
endfunction


function! neomake#makers#ft#rust#rustc() abort
    return {
        \ 'exe': 'cargo',
        \ 'args': ['rustc', '--', '-Zno-trans'],
        \ 'append_file': 0,
        \ 'errorformat':
            \ '%-G%f:%s:,' .
            \ '%f:%l:%c: %trror: %m,' .
            \ '%f:%l:%c: %tarning: %m,' .
            \ '%f:%l:%c: %m,'.
            \ '%f:%l: %trror: %m,'.
            \ '%f:%l: %tarning: %m,'.
            \ '%f:%l: %m',
        \ }
endfunction
