" plugin/rc/fzf
scriptencoding utf-8



command! -bang -nargs=? -complete=dir Files
    \ call fzf#vim#files(<q-args>,
        \ {'options': [
            \ '--layout=reverse',
            \ '--info=inline',
            \ '--preview',
            \ '~/.vim/bundle/fzf.vim/bin/preview.sh {}',
            \ ],
        \ },
    \ <bang>0)

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
  \ 'ctrl-v': 'vsplit'
\ }


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
