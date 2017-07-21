" autoload/z/constants/rc
scriptencoding utf-8


func! z#constants#rc#RcFiles(...) abort
    let l:opts = z#util#Opts(s:RcFiles_opts, get(a:, '1', {}))

    let l:rcfiles = {}

    for [l:rcfile_name, l:rcfile] in items(l:opts.rcfiles)
        if has('nvim')
            let l:rcfile_path = globpath(g:dotvim_f, 'rc/'.l:rcfile.'.vim', 0, 1)
        else
            let l:rcfile_path = [globpath(g:dotvim_f, 'rc/'.l:rcfile.'.vim')]
        endif
        let l:rcfiles[l:rcfile_name] = get(l:rcfile_path, 0, '/dev/null')
    endfor

    return l:rcfiles
endfunc

let s:RcFiles_opts = {
    \ 'rcfiles' : {
        \  'colorsc'    : 'colorsc'
        \ ,'keys'       : 'keys'
        \ ,'plugins'    : 'plugins'
        \ ,'settings'   : 'settings'
        \ ,'statusline' : 'statusline'
    \ },
\ }
