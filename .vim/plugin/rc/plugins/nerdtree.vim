" nerdtree
scriptencoding utf-8


let g:NERDTreeWinPos                    = 'left'
let g:NERDTreeAutoCenter                = 1
let g:NERDTreeCaseSensitiveSort         = 0
let g:NERDTreeHighlightCursorline       = 1
let g:NERDTreeBookmarksFile             = g:dotvim_f.'/.cache/NERDTree.bookmarks'
let g:NERDTreeMouseMode                 = 2
let g:NERDTreeShowHidden                = 1
let g:NERDTreeMinimalUI                 = 1
let g:NERDTreeHijackNetrw               = 1
let g:NERDTreeRespectWildIgnore         = 1
let g:NERDTreeCascadeOpenSingleChildDir = 1
let g:NERDTreeAutoDeleteBuffer          = 1
"let g:NERDTreeChDirMode                 = 2
let g:NERDTreeWinSize                   = 28

let g:NERDTreeCreatePrefix = 'silent keepa keepj keepp keepm'

let g:NERDTreeIgnore    = [    '\~$' ,
    \ '' . '\.git'             . '$' . '[[dir]]'  ,
    \ '' . '\.hg'              . '$' . '[[dir]]'  ,
    \ '' . '\.svn'             . '$' . '[[dir]]'  ,
    \ '' . 'node_modules'      . '$' . '[[dir]]'  ,
    \ '' . 'bower_components'  . '$' . '[[dir]]'  ,
    \ ''  . '\.o'              . '$' . '[[file]]' ,
    \ ''  . '\.obj'            . '$' . '[[file]]' ,
    \ ''  . '\.so'             . '$' . '[[file]]' ,
    \ ''  . '\.exe'            . '$' . '[[file]]' ,
    \ ''  . '\.dmg'            . '$' . '[[file]]' ,
    \ ''  . '\.swap'           . '$' . '[[file]]' ,
    \ ''  . '\.class'          . '$' . '[[file]]' ,
    \ ''  . '\.pyc'            . '$' . '[[file]]' ,
    \ ''  . '\.tar'            . '$' . '[[file]]' ,
    \ ''  . '\.bz'             . '$' . '[[file]]' ,
    \ ''  . '\.gz'             . '$' . '[[file]]' ,
    \ ''  . '\.xz'             . '$' . '[[file]]' ,
    \ ''  . '\.zip'            . '$' . '[[file]]' ,
    \ ''  . '\.DS_Store'       . '$' . '[[file]]' ,
    \ ''  . '%'                . ''  . '[[file]]' ,
\ ]


"let g:NERDTreeIndicatorMapCustom = {
    "\ 'Modified'  : '✹',
    "\ 'Staged'    : '✚',
    "\ 'Untracked' : '✭',
    "\ 'Renamed'   : '➜',
    "\ 'Unmerged'  : '═',
    "\ 'Deleted'   : '✖',
    "\ 'Dirty'     : '✗',
    "\ 'Clean'     : '✔︎',
    "\ 'Unknown'   : '?',
"\ }


" autocommand - auto-cwd (BufEnter *)
aug MyNERDTree
    au!
    "au BufEnter * silent! call z#nerdtree#AutoCwd()
    "au TabNew * call <SID>MyNERDTreeToggle()
aug END


nnoremap <silent> <Leader>n<Space>     :call z#nerdtree#Toggle()<CR>
map      <silent> <Leader><S-n><Space> <plug>NERDTreeTabsFind<CR>
