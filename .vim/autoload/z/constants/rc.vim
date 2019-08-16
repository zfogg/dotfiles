" autoload/z/constants/rc
scriptencoding utf-8


func! z#constants#rc#RcFiles(...) abort
    let l:opts = z#util#Opts(s:RcFiles_opts, get(a:, '1', {}))

    let l:rcfiles = {}

    for [l:rcfile_name, l:rcfile] in items(l:opts.rcfiles)
        let l:rcfile_path = globpath(g:dotvim_f, l:rcfile.'.vim', 0, 1)
        let l:rcfiles[l:rcfile_name] = get(l:rcfile_path, 0, '/dev/null')
    endfor

    return l:rcfiles
endfunc

let s:RcFiles_opts = {
    \ 'rcfiles' : {
        \  'plugins'    : 'plugins'
        \ ,'settings'   : 'plugins/1.settings'
        \ ,'statusline' : 'plugins/2.statusline'
        \ ,'keys'       : 'plugins/3.keys'
        \ ,'colorsc'    : 'plugins/4.colorsc'
    \ },
\ }
