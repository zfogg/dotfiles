" lightline
scriptencoding utf-8


aug rc_plugins_lightline
    au!
    au User NeomakeCountsChanged,NeomakeMakerFinished
        \ :call z#lightline#OnNeomakeCountsChanged()
aug END


let g:lightline = {
    \ 'colorscheme': 'jellybeans'
    \,'separator'    : { 'left': '', 'right': '' }
    \,'subseparator' : { 'left': '', 'right': '' }
    \,'active': {
        \ 'left': [
            \ ['mode', 'paste']
            \,['fugitive', 'filename']
        \ ],
        \ 'right': [
            \ ['lineinfo', 'neomake']
            \,['percent']
            \,['fileformat', 'fileencoding', 'filetype']
        \ ],
    \ },
    \ 'component_function': {
        \ 'readonly'     : 'z#lightline#Readonly'
        \,'modified'     : 'z#lightline#Modified'
        \,'fugitive'     : 'z#lightline#Fugitive'
        \,'filename'     : 'z#lightline#Filename'
        \,'fileformat'   : 'z#lightline#Fileformat'
        \,'filetype'     : 'z#lightline#Filetype'
        \,'fileencoding' : 'z#lightline#Fileencoding'
        \,'mode'         : 'z#lightline#Mode'
    \ },
    \ 'component_expand': {
        \ 'neomake' : 'z#lightline#Neomake'
    \ },
    \ 'component_type': {
        \ 'neomake' : 'error'
    \ },
\ }
