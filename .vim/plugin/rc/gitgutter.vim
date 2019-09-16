" plugin/rc/gitgutter
scriptencoding utf-8


let g:gitgutter_enabled              = 1
let g:gitgutter_map_keys             = 1
let g:gitgutter_highlight_lines      = 1
let g:gitgutter_highlight_linenrs    = 1
let g:gitgutter_async                = 1
let g:gitgutter_diff_args            = '-w'
let g:gitgutter_max_signs            = 1024
let g:gitgutter_preview_win_floating = 1
"let g:gitgutter_diff_relative_to     = 'working_tree'

"let g:gitgutter_grep_command    = &grepprg
if executable('/usr/bin/rg')
    let g:gitgutter_grep = '/usr/bin/rg --no-config --no-heading --color=never --no-line-number --ignore'
else
    let g:gitgutter_grep = '/usr/bin/grep --color=never'
endif


nnoremap <Leader>gg<Space>  :GitGutterToggle<CR>
nnoremap <Leader>ggl<Space> :GitGutterLineHighlightsToggle<CR>

nmap             -          <Plug>(GitGutterPrevHunk)<Bar><Plug>(GitGutterPreviewHunk)
nmap             =          <Plug>(GitGutterNextHunk)<Bar><Plug>(GitGutterPreviewHunk)

nmap     <Leader>ggw        <Plug>(GitGutterStageHunk)
nmap     <Leader>ggu        <Plug>(GitGutterUndoHunk)
nmap     <Leader>ggd        <Plug>(GitGutterPreviewHunk)

" replace the `=` mapping
nnoremap <Leader>=          =


aug RcPlugin__gitgutter
    au!
    "au ColorScheme,VimEnter * highlight link GitGutterAddLine DiffAdded
    au ColorScheme,VimEnter * highlight GitGutterAddLine               ctermbg=18               guibg=#282a2e gui=bold
    au ColorScheme,VimEnter * highlight GitGutterChangeLine            ctermbg=18               guibg=#28322d gui=bold,italic,underline
    au ColorScheme,VimEnter * highlight GitGutterDeleteLine ctermfg=7  ctermbg=0  guifg=#773333 guibg=#e0e0e0 gui=italic,standout
    "au ColorScheme,VimEnter * highlight link GitGutterDeleteLine DiffDelete
aug END
