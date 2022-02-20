" autoload/z/util


func! z#util#globpathL(path, glob) abort
    return globpath(expand(a:path), a:glob, 0, 1)
endfunc


fun! z#util#TrimWhitespace() abort
    let l:save = winsaveview()
    keepjumps keeppatterns %s/\s\+$//e
    call winrestview(l:save)
endfun


func! z#util#Helptags() abort
    packadd packer.nvim
    packadd vimball
    for p in glob('~/.local/share/nvim/site/pack/packer/opt/*', 1, 1)
        exe 'packadd ' . fnamemodify(p, ':t')
    endfor
    helptags ALL
endfun

