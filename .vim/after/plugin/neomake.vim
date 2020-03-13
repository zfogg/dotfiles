" after/plugin/neomake
scriptencoding utf-8


"\ 'TextChanged':  {'delay': 0},
if has('win32')
  call neomake#configure#automake({
  \ 'BufWritePost': {'delay': 0},
  \ 'BufWinEnter':  {'delay': 180},
  \ })
else
  call neomake#configure#automake({
  \ 'TextChanged':  {'delay': 200},
  \ 'InsertLeave':  {'delay': 130},
  \ 'BufWritePost': {'delay': 0},
  \ 'BufWinEnter':  {'delay': 180},
  \ })
endif

"call neomake#configure#automake('rwn', 350)
