" plugin/rc/fzf
scriptencoding utf-8


if !PHas('fzf.vim') | finish | endif


let g:fzf_preview_window = ['right:60%', 'ctrl-/']
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
  aug fzf-custom
    au!
    autocmd! FileType fzf
    autocmd  FileType fzf set laststatus=0 noshowmode noruler
        \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler
  aug END
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


" FIXME: from plugin/3.keys.vim
" files - search, sort, find, open {{{
"    nnoremap <C-f>     :Rg! 
"    nnoremap <Leader>f :execute ':Rg! <C-r><C-w>'<Cr>
"    nnoremap <C-g>     :GFiles<Cr>
"    nnoremap <C-p>     :Files<Cr>

"    " Mapping selecting mappings
"    nmap <leader><tab> <plug>(fzf-maps-n)
"    xmap <leader><tab> <plug>(fzf-maps-x)
"    omap <leader><tab> <plug>(fzf-maps-o)

"    " Insert mode completion
"    imap <c-x><c-k> <plug>(fzf-complete-word)
"    imap <c-x><c-f> <plug>(fzf-complete-path)
"    imap <c-x><c-l> <plug>(fzf-complete-line)
" }}}
