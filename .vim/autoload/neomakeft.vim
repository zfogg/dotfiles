" autoload/neomakeft/z
scriptencoding utf-8


func! neomakeft#SetupFt(ft, ...) abort
    let l:opts = z#util#Opts(s:setupFt_opts, get(a:, '1', v:null))
    let l:neomakers_var = 'g:neomake_'.a:ft.'_enabled_makers'
    exe 'let '.l:neomakers_var.' = '.string(l:opts.makers)
    if l:opts.autocmds
        call <SID>MakeAuFt(a:ft)
    endif
endfunc

let s:setupFt_opts = {
    \ 'autocmds' : v:true,
    \ 'makers'   : [],
\ }


func! <SID>MakeAuFt(ft) abort
    let l:makeAuBuf_call = 'neomakeft#MakeAuBuf('.string(a:ft).')'
    let l:auFt_var = 'neomakeft#AuFt_'.&ft
    exe 'let g:'.l:auFt_var.' = '.string(get(g:, l:auFt_var, s:autocmds_saneOpts))
    exe 'aug rc_z_neomakeft_'.a:ft.'_au_ft'
        au!
        exe 'au FileType '.a:ft.' call '.l:makeAuBuf_call
    aug END
endfunc


func! neomakeft#MakeAuBuf(ft) abort
    exe 'aug rc_z_neomakeft_'.a:ft.'_au_buf'
        au!
        for [l:ev, l:opts] in items(get(g:, 'neomakeft#AuFt_'.&ft, s:autocmds_saneOpts))
            let l:evs = join(l:opts.events, ',')
            exe 'au '.l:evs.' <buffer> '.l:opts.command
        endfor
    aug END
endfunc


let g:neomakeft#Autocmds_SaneOpts = {
    \ 'read': {
        \ 'command': 'Neomake'
        \,'events': [
            \ 'BufReadPost'
            \,'FileReadPost'
        \ ],
    \},'write': {
        \ 'command': 'Neomake'
        \,'events': [
            \'BufWritePost'
        \],
    \},'change': {
        \ 'command': 'update | Neomake'
        \,'events': [
            \ 'InsertChange'
            \,'TextChanged'
        \],
    \},
\}

let s:autocmds_saneOpts = deepcopy(g:neomakeft#Autocmds_SaneOpts)
