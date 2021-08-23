" after/plugin/indent_blankline
scriptencoding utf-8


" {{{ indent-blanklinke.nvim
let g:indent_blankline_show_trailing_blankline_indent = v:true

"let g:indent_blankline_char = '▏'
let g:indent_blankline_char_list = ['┃', '┇', '⎪', '┇', '▏', '⎹']
let g:indent_blankline_show_end_of_line = v:true
let g:indent_blankline_use_treesitter = v:false
let g:indent_blankline_filetype_exclude = ["help","dashboard","dashpreview","NvimTree","coc-explorer","startify","vista","sagahover","nerdtree"]

 "let g:indent_blankline_context_patterns = [
		 "\ 'class', 'function', 'method',
		 "\ 'if_statement', 'else_clause', 'jsx_element', 'jsx_self_closing_element',
		 "\ 'try_statement', 'catch_clause',
		 "\ ]

highlight IndentBlanklineChar1                    guifg=#ca6b9f gui=nocombine
highlight IndentBlanklineChar2                    guifg=#a1b56c gui=nocombine
highlight IndentBlanklineChar3                    guifg=#f7ca88 gui=nocombine
highlight IndentBlanklineChar4                    guifg=#7cafc2 gui=nocombine
highlight IndentBlanklineChar5                    guifg=#86c1b9 gui=nocombine
highlight IndentBlanklineSpaceChar1 guibg=#181818               gui=nocombine
highlight IndentBlanklineSpaceChar2 guibg=#181818               gui=nocombine
highlight IndentBlanklineSpaceChar3 guibg=#181818               gui=nocombine
highlight IndentBlanklineSpaceChar4 guibg=#181818               gui=nocombine
highlight IndentBlanklineSpaceChar5 guibg=#181818               gui=nocombine

let g:indent_blankline_char_highlight_list       = ['IndentBlanklineChar1', 'IndentBlanklineChar2', 'IndentBlanklineChar3', 'IndentBlanklineChar4', 'IndentBlanklineChar5']
let g:indent_blankline_space_char_highlight_list = ['IndentBlanklineSpaceChar1', 'IndentBlanklineSpaceChar2', 'IndentBlanklineSpaceChar3', 'IndentBlanklineSpaceChar4', 'IndentBlanklineSpaceChar5']
" }}}

