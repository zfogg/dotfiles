" filetype=c,cpp


syn region foldPreprocessorIf start="^\#if" end="^\#endif" transparent fold
setl sw=4 ts=4 sts=4
setl fdm=marker fmr={,}
