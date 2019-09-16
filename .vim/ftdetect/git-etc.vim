" ftdetect/gitdiff
scriptencoding utf-8


autocmd BufNewFile,BufRead,StdinReadPost *
    \ if getline(1) =~ '^diff --git a/.\+ b/.\+$' |
    \   set ft=git.gitdiff |
    \ endif

autocmd BufNewFile,BufRead,StdinReadPost *
    \ if getline(1) =~ '^commit ' && getline(2) =~ '^Author: ' && getline(3) =~ '^Date: ' |
    \   set ft=gitsendemail.gitcommit |
    \ endif
