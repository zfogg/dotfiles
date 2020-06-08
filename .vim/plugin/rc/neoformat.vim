" plugin/rc/neoformat
scriptencoding utf-8


if z#util#HasPlugin('neoformat')
    let g:neoformat_only_msg_on_error = 1
    "let g:neoformat_try_formatprg = 1

    "\ 'args': ['-s 4', '-E'],
    "\ 'replace': 1 " replace the file, instead of updating buffer (default: 0),
    "\ 'stdin': 1, " send data to stdin of formatter (default: 0)
    "\ 'env': ["DEBUG=1"], " prepend environment variables to formatter command
    "\ 'valid_exit_codes': [0, 23],
    "\ 'no_append': 1,

    aug RcPlugin__neoformat
        au!
        "au BufWritePre *.js,*.jsx,*.ts,*.tsx Neoformat
    aug END
endif
