" plugin/rc/fzf
scriptencoding utf-8


let g:fzf_preview_window = 'right:60%'
let g:fzf_buffers_jump = 1
if exists('$FZF_HISTORY_DIR')
  let g:fzf_history_dir = expand('$FZF_HISTORY_DIR')
else
  let g:fzf_history_dir = $HOME . '/.fzf-history'
endif
call mkdir(g:fzf_history_dir, 'p')

command! -bang -nargs=? -complete=dir Files
  \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(
    \ {'options': [
      \ ],
    \ },
  \ ), <bang>0)

let g:fzf_colors = {
    \ 'fg':      ['fg', 'Normal'],
    \ 'bg':      ['bg', 'Normal'],
    \ 'hl':      ['fg', 'Comment'],
    \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
    \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
    \ 'hl+':     ['fg', 'Statement'],
    \ 'info':    ['fg', 'PreProc'],
    \ 'border':  ['fg', 'Ignore'],
    \ 'prompt':  ['fg', 'Conditional'],
    \ 'pointer': ['fg', 'Exception'],
    \ 'marker':  ['fg', 'Keyword'],
    \ 'spinner': ['fg', 'Label'],
    \ 'header':  ['fg', 'Comment']
\ }

let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-s': 'split',
  \ 'ctrl-v': 'vsplit',
\ }

" hide status line
if has('nvim') && !exists('g:fzf_layout')
  autocmd! FileType fzf
  autocmd  FileType fzf set laststatus=0 noshowmode noruler
    \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler
endif

if executable('rg')
  let s:fzf_find_command = 'rg'
        \ . ' --fixed-strings'
        \ . ' --ignore-case'
        \ . ' --follow'
        \ . ' --color "always"'

  if exists('$TMUX')
      command! -bang -nargs=* Find
        \ call fzf#vim#grep(s:fzf_find_command . ' '.shellescape(<q-args>).'| tr -d "\017"',
        \ 1,
        \ <bang>0)
  else
      command! -bang -nargs=* Find
        \ call fzf#vim#grep(s:fzf_find_command . ' '.shellescape(<q-args>),
        \ 1,
        \ <bang>0)
  endif
endif
