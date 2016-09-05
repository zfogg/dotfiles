" filetype=c


syn region foldPreprocessorIf start="^\#if" end="^\#endif" transparent fold

setl sw=4 ts=4 sts=4
setl fdm=marker fmr={,}

"echo 'ft - '.&ft

"au BufReadPost,FileReadPost,BufWritePost *.c,*.h          Neomake
"au InsertChange,TextChanged              *.c,*.h update | Neomake
