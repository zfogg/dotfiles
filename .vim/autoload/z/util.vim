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
    elseif has('win32')
        let s:wintmp = expand($TEMP)
        let l:std_dirs += [
            \ s:wintmp .                 l:post,
        \ ]
    endif
    if has('unix')
        let l:std_dirs += [
            \ '/var/tmp'     . l:post,
            \ '/tmp'         . l:post,
        \ ]
    endif
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


fun! z#util#TrimWhitespace() abort
    let l:save = winsaveview()
    keepjumps keeppatterns %s/\s\+$//e
    call winrestview(l:save)
endfun


fun! z#util#HasPlugin(name) abort
    let l:name  = get(a:, 'name', '')
    let l:plugs = get(g:, 'plugs', {})
    return has_key(l:plugs, l:name)
endfun

fun! z#util#IfPlugin(cond, ...) abort
    let l:opts = get(a:000, 0, {})
    let l:cond = get(a:, 'cond', v:false)
    return l:cond ? l:opts : extend(l:opts, { 'on': [], 'for': [] })
endfun

fun! z#util#GetSetEnv(env_key, env_val) abort
    let l:environ = environ()
    let l:env_key = get(a:, 'env_key', '')
    let l:env_val = get(a:, 'env_val', '')
    try
        if empty(l:env_key) | throw "EmptyEnvKey" | endif
        let l:env_val = get(l:environ, l:env_key, l:env_val)
    catch /^EmptyEnvKey/
        echom "empty env_key = '".l:env_key."'"
    finally
        if !has_key(l:environ, l:env_key)
            exec "let $".l:env_key." = '".l:env_val."'"
        endif
    endtry
    return eval('$'.l:env_key)
endfun


" FIXME
fun! z#util#Shiftwidth() abort
    " INFO: :help 'shiftwidth()'
    let l:sw =<< trim END
        if exists('*shiftwidth')
            func s:sw()
                return shiftwidth()
            endfunc
        else
            func s:sw()
                return &sw
            endfunc
        endif
    END
    let l:swt = map(copy(l:sw), {_, v -> v})
    return join(l:swt, "\n")
endfun

