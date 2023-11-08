" nerdcommenter


"let g:NERDMenuMode               = 1
let g:NERDSpaceDelims            = 0
let g:NERDRemoveExtraSpaces      = 1
let g:NERDCompactSexyComs        = 0

" 'NERDDefaultAlign'
" Values: 'none', 'left', 'start', 'both'
" Default 'none'.
"let g:NERDAlignDefault           = 'none' " INFO: lol the variable name has a typo... remember?
let g:NERDDefaultAlign           = 'both'

function! Yo() abort
   echom 'yo'
endfunc

let g:NERDTrimTrailingWhitespace = 1

"" `&filetype` settings
let g:NERDAltDelims_haskell    = 1
