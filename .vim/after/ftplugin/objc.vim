" filetype=objc


"au! BufReadPost,FileReadPost,BufWritePost *.m,*.h          Neomake
"au! InsertChange,TextChanged              *.m,*.h update | Neomake
