" filetype=objcpp


"au! BufReadPost,FileReadPost,BufWritePost *.mm,*.hh          Neomake
"au! InsertChange,TextChanged              *.mm,*.hh update | Neomake
