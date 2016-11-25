" nerdtree
scriptencoding utf-8


let g:NERDTreeAutoCenter                = 1
let g:NERDTreeAutoCenterThreshold       = 5

let g:NERDTreeCaseSensitiveSort         = 0
let g:NERDTreeSortHiddenFirst           = 1
let g:NERDTreeShowHidden                = 1

let g:NERDTreeChDirMode                 = 2
let g:NERDTreeMouseMode                 = 2
let g:NERDTreeHijackNetrw               = 1
let g:NERDTreeCascadeSingleChildDir     = 1
let g:NERDTreeCascadeOpenSingleChildDir = 1
let g:NERDTreeAutoDeleteBuffer          = 1

let g:NERDTreeHighlightCursorline       = 1
let g:NERDTreeStatusline                = '%{matchstr(getline("."), "[0-9A-Za-z_/].*")}'
let g:NERDTreeWinPos                    = 'left'
let g:NERDTreeWinSize                   = 29
let g:NERDTreeMinimalUI                 = 1

let g:NERDTreeCreatePrefix  = 'silent! keepald keepjumps keepmarks keeppatterns'
let g:NERDTreeBookmarksFile = g:dotvim_f.'/.cache/NERDTree.bookmarks'

let g:NERDTreeRespectWildIgnore = 1
let g:NERDTreeIgnore            = [
    \ '' . '\~'               . '$'              ,
    \ '' . '.swp'             . '$' . '[[file]]' ,
    \ '' . '.DS_Store'        . '$' . '[[file]]' ,
    \ '' . '.o'               . '$' . '[[file]]' ,
    \ '' . '.obj'             . '$' . '[[file]]' ,
    \ '' . '.so'              . '$' . '[[file]]' ,
    \ '' . '.exe'             . '$' . '[[file]]' ,
    \ '' . '.dmg'             . '$' . '[[file]]' ,
    \ '' . '.swap'            . '$' . '[[file]]' ,
    \ '' . '.class'           . '$' . '[[file]]' ,
    \ '' . '.pyc'             . '$' . '[[file]]' ,
    \ '' . '.tar'             . '$' . '[[file]]' ,
    \ '' . '.bz'              . '$' . '[[file]]' ,
    \ '' . '.gz'              . '$' . '[[file]]' ,
    \ '' . '.xz'              . '$' . '[[file]]' ,
    \ '' . '.zip'             . '$' . '[[file]]' ,
    \ '' . '.git'             . '$' . '[[dir]]'  ,
    \ '' . '.hg'              . '$' . '[[dir]]'  ,
    \ '' . '.svn'             . '$' . '[[dir]]'  ,
    \ '' . 'node_modules'     . '$' . '[[dir]]'  ,
    \ '' . 'bower_components' . '$' . '[[dir]]'  ,
\ ]


map <Leader><S-n><Space> <Plug>NERDTreeTabsToggle<CR>
map <Leader>n<Space>     <Plug>NERDTreeMirrorToggle<CR>
map <Leader>nn           <Plug>NERDTreeTabsFind<CR>
