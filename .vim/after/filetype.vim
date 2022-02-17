" after/filetype
scriptencoding utf-8


let g:c_syntax_for_h = 1


"function! LC_maps()
    "if has_key(g:LanguageClient_serverCommands, &filetype)
        "nmap <F5>          <Plug>(lcn-menu)
        "" Or map each action separately
        "nmap <silent> K    <Plug>(lcn-hover)
        "nmap <silent> gd   <Plug>(lcn-definition)
        "nmap <silent> <F2> <Plug>(lcn-rename)
    "endif
"endfunction

"aug rc_after_filetype_LC_maps
    "au!
    "au FileType * call LC_maps()
"aug END
