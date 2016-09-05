" filetype=cpp


syn region foldPreprocessorIf start="^\#if" end="^\#endif" transparent fold

setl sw=4 ts=4 sts=4
setl fdm=marker fmr={,}

"au! BufReadPost,FileReadPost,BufWritePost *.cpp,*.hpp          Neomake
"au! InsertChange,TextChanged              *.cpp,*.hpp update | Neomake
