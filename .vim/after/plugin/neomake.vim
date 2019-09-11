" after/plugin/neomake
scriptencoding utf-8


"\ 'TextChanged':  {'delay': 0},
call neomake#configure#automake({
\ 'TextChanged':  {'delay': 200},
\ 'InsertLeave':  {'delay': 500},
\ 'BufWritePost': {'delay': 0},
\ 'BufWinEnter':  {'delay': 200},
\ })

"call neomake#configure#automake('rwn', 350)
