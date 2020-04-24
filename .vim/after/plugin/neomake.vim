" after/plugin/neomake
scriptencoding utf-8

if !has('nvim') | finish | endif

"\ 'TextChanged':  {'delay': 0},
if has('win32')
  call neomake#configure#automake({
  \ 'BufWritePost': {'delay': 0},
  \ 'BufWinEnter':  {'delay': 180},
  \ })
else
  call neomake#configure#automake({
  \ 'TextChanged':  {'delay': 700},
  \ 'InsertLeave':  {'delay': 1000},
  \ 'BufWritePost': {'delay': 0},
  \ 'BufWinEnter':  {'delay': 500},
  \ })
endif

"call neomake#configure#automake('rwn', 350)
