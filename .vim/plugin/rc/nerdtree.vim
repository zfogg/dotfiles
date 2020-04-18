" plugin/rc/nerdtree
scriptencoding utf-8


if exists('g:vimpager') | finish | endif


let g:NERDTreeAutoCenter                = 1
let g:NERDTreeAutoCenterThreshold       = 5

let g:NERDTreeCaseSensitiveSort         = 0
let g:NERDTreeSortHiddenFirst           = 1
let g:NERDTreeShowHidden                = 1


let g:NERDTreeChDirMode                 = 1
let g:NERDTreeMouseMode                 = 2
let g:NERDTreeHijackNetrw               = 1
let g:NERDTreeCascadeSingleChildDir     = 1
let g:NERDTreeCascadeOpenSingleChildDir = 1
let g:NERDTreeAutoDeleteBuffer          = 1

let g:NERDTreeHighlightCursorline       = 0
let g:NERDTreeWinPos                    = 'left'
let g:NERDTreeWinSize                   = 27
let g:NERDTreeMinimalUI                 = 1

let g:NERDTreeStatusline                = ''
            \.'%{matchstr(getline("."), "[0-9A-Za-z_/].*")}'
            \.'%='
            \.' %=  '
            \.'%2l'

let g:NERDTreeCreatePrefix  = 'silent! keepald keepjumps keepmarks keeppatterns'
let s:dotvim_f = get(g:, 'dotvim_f', $HOME.'/.vim')
let g:NERDTreeBookmarksFile = s:dotvim_f.'/.cache/NERDTree.bookmarks'

let g:NERDTreeRespectWildIgnore = 1
let g:NERDTreeIgnore            = [
    \ '' . '\~'               . '$'              ,
    \ '' . '\.swp'            . '$' . '[[file]]' ,
    \ '' . '\.DS_Store'       . '$' . '[[file]]' ,
    \ '' . '\.o'              . '$' . '[[file]]' ,
    \ '' . '\.obj'            . '$' . '[[file]]' ,
    \ '' . '\.so'             . '$' . '[[file]]' ,
    \ '' . '\.exe'            . '$' . '[[file]]' ,
    \ '' . '\.dmg'            . '$' . '[[file]]' ,
    \ '' . '\.swap'           . '$' . '[[file]]' ,
    \ '' . '\.class'          . '$' . '[[file]]' ,
    \ '' . '\.pyc'            . '$' . '[[file]]' ,
    \ '' . '\.tar'            . '$' . '[[file]]' ,
    \ '' . '\.bz'             . '$' . '[[file]]' ,
    \ '' . '\.gz'             . '$' . '[[file]]' ,
    \ '' . '\.xz'             . '$' . '[[file]]' ,
    \ '' . '\.rustfmt'        . '$' . '[[file]]' ,
    \ '' . '\.zip'            . '$' . '[[file]]' ,
    \ '' . '\.zwc'            . '$' . '[[file]]' ,
    \ '' . '\.zwc.old'        . '$' . '[[file]]' ,
    \ '' . '\.js.map'         . '$' . '[[file]]' ,
    \ '' . '\.git'            . '$' . '[[dir]]'  ,
    \ '' . '\.hg'             . '$' . '[[dir]]'  ,
    \ '' . '\.svn'            . '$' . '[[dir]]'  ,
    \ '' . 'node_modules'     . '$' . '[[dir]]'  ,
    \ '' . 'bower_components' . '$' . '[[dir]]'  ,
    \ '' . 'vendor'           . '$' . '[[dir]]'  ,
    \ '' . '__pycache__'      . '$' . '[[dir]]'  ,
    \ '' . 'dist'             . '$' . '[[dir]]'  ,
\ ]


"nnoremap <Leader><S-n><Space> <Plug>NERDTreeTabsToggle<CR>
nnoremap <Leader>n<Space>     :NERDTreeToggle<CR>:wincmd p<CR>
nnoremap <Leader>nn           :call z#nerdtree#AutoCwd()<CR>
