" autoload/z/util


func! z#util#globpathL(path, glob) abort
    return globpath(expand(a:path), a:glob, 0, 1)
endfunc


func! z#util#TempDirs(post, ...) abort
    let l:post = exists('a:post') ? a:post : ''
    let l:pre_dirs = a:0 > 0
        \ ? map(copy(a:000), 'v:val . l:post')
        \ : []
    let l:std_dirs = []
    if has('osx')
        let s:osxtmp = get(s:, 'mytmpdir', system('echo -n "${TMPDIR%/}"'))
        let l:std_dirs += [
            \ s:osxtmp .                 l:post,
        \ ]
    endif
    let l:std_dirs += [
        \ '/var/tmp'     . l:post,
        \ '/tmp'         . l:post,
    \ ]
    let l:out_dirty = join(l:pre_dirs + l:std_dirs, ',')
    return escape(l:out_dirty, '\')
endfunc


func! z#util#Opts(defaults, ...) abort
    let l:opts = deepcopy(get(a:000, '0', {}))
    for [l:k, l:v] in items(a:defaults)
        let l:opts[l:k] = get(l:opts, l:k, l:v)
    endfor
    return l:opts
endfunc
