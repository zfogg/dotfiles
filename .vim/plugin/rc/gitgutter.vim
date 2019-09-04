" gitgutter


let g:gitgutter_enabled         = 1
let g:gitgutter_map_keys        = 1
let g:gitgutter_highlight_lines = 0
let g:gitgutter_async           = 1
let g:gitgutter_realtime        = 1
let g:gitgutter_eager           = 1
let g:gitgutter_diff_args       = '-w'
let g:gitgutter_max_signs       = 2048

let g:gitgutter_preview_win_floating = 1

"let g:gitgutter_grep_command    = &grepprg


nnoremap <Leader>gg<Space>  :GitGutterToggle<CR>
nnoremap <Leader>ggl<Space> :GitGutterLineHighlightsToggle<CR>
nmap             -          <Plug>GitGutterPrevHunk<Bar><Plug>GitGutterPreviewHunk
nmap             =          <Plug>GitGutterNextHunk<Bar><Plug>GitGutterPreviewHunk
nmap     <Leader>ggw        <Plug>GitGutterStageHunk
nmap     <Leader>ggu        <Plug>GitGutterUndoHunk
nmap     <Leader>ggd        <Plug>GitGutterPreviewHunk
" replace the `=` mapping
nnoremap <Leader>=          =
