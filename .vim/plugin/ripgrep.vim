" ripgrep
scriptencoding utf-8


if executable('rg')
    let g:rg_prg    = 'rg --vimgrep --ignore-case --ignore-file='.$HOME.'/.rgignore'
    let &grepprg    = g:rg_prg
    let &grepformat = '%f:%l:%c:%m'
else
    let &grepprg = 'grep --color=never -e --exclude-dir .git -nrI $* . /dev/null'
endif
