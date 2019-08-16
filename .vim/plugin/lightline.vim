" lightline
scriptencoding utf-8


finish " FIXME: not using this plugin


let g:lightline = {
    \ 'colorscheme': 'Tomorrow_Night'
    \,'separator'    : { 'left': '', 'right': '' }
    \,'subseparator' : { 'left': '', 'right': '' }
    \,'active': {
        \ 'left': [
            \ ['mode', 'paste']
            \,['fugitive']
            \,['filename']
        \ ],
        \ 'right': [
            \ ['lineinfo']
            \,['percent', 'neomake']
            \,['filetype']
        \ ],
    \ },
    \ 'component_function': {
        \ 'readonly'     : 'z#lightline#Readonly'
        \,'modified'     : 'z#lightline#Modified'
        \,'fugitive'     : 'z#lightline#Fugitive'
        \,'filename'     : 'z#lightline#Filename'
        \,'lineinfo'     : 'z#lightline#Lineinfo'
        \,'percent'      : 'z#lightline#Percent'
        \,'fileformat'   : 'z#lightline#Fileformat'
        \,'filetype'     : 'z#lightline#Filetype'
        \,'fileencoding' : 'z#lightline#Fileencoding'
        \,'mode'         : 'z#lightline#Mode'
    \ },
    \ 'component_expand': {
        \ 'lineinfo' : 'z#lightline#Lineinfo'
        \,'neomake'  : 'z#lightline#Neomake'
    \ },
    \ 'component_type': {
        \ 'neomake' : 'error'
    \ },
\ }


aug RcPlugin__lightline_neomake
    au!
    au User NeomakeCountsChanged,NeomakeMakerFinished
        \ call z#lightline#OnNeomakeCountsChanged()
aug END
