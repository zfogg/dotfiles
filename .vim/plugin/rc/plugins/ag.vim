" ag
scriptencoding utf-8


if executable('ag')
    let g:ag_prg    = 'ag --vimgrep --nogroup --hidden -p '.$HOME.'/.agignore $*'
    let &grepprg    = g:ag_prg
    let &grepformat = '%f:%l:%c:%m'
else
    let &grepprg = 'grep --color=never -e --exclude-dir .git -nrI $* . /dev/null'
endif
