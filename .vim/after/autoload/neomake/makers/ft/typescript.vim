" after/autoload/neomake/makers/ft/typescript
scriptencoding utf-8


function! neomake#makers#ft#typescript#EnabledMakers() abort
    return ['tsc', 'eslint']
endfunction

