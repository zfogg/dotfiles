" after/ftplugin/javascript
scriptencoding utf-8


setl fdm=syntax
    \ iskeyword+=$


aug RcPlugin__neomake
    au!
    au BufEnter *.js let b:neomake_javascript_eslint_exe = nrun#Which('eslint')
aug END

