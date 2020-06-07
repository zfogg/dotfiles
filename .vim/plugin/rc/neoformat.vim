" plugin/rc/neoformat
scriptencoding utf-8


if z#util#HasPlugin('neoformat')
    "let g:neoformat_only_msg_on_error = 1
    let g:neoformat_try_formatprg = 1

    function! NeoformatPrettier() abort
        if z#util#HasPlugin('nrun.vim')
            let s:prettier_exe = nrun#Which('prettier')
            let g:neoformat_javascript_prettier = {
                \ 'exe': s:prettier_exe,
            \ }
            let g:neoformat_typescript_prettier = {
                \ 'exe': s:prettier_exe,
            \ }
        endif
        let g:neoformat_enabled_javascript = ['prettier']
        let g:neoformat_enabled_typescript = ['prettier']
    endfunc

    "\ 'args': ['-s 4', '-E'],
    "\ 'replace': 1 " replace the file, instead of updating buffer (default: 0),
    "\ 'stdin': 1, " send data to stdin of formatter (default: 0)
    "\ 'env': ["DEBUG=1"], " prepend environment variables to formatter command
    "\ 'valid_exit_codes': [0, 23],
    "\ 'no_append': 1,

    aug RcPlugin__neoformat
        au!
        au BufWritePre *.js,*.jsx,*.ts,*.tsx undojoin | Neoformat
    aug END
endif
