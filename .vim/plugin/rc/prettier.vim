" plugin/rc/prettier
scriptencoding utf-8


if z#util#HasPlugin('vim-prettier')
    let g:prettier#autoformat_config_present = 1
    let g:prettier#exec_cmd_async = 1
    let g:prettier#quickfix_auto_focus = 0
    aug RcPlugin__prettier
        au!
        "au BufWritePre *.js,*.jsx,*.mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.graphql,*.md,*.vue,*.yaml,*.html
        "au BufWritePre *.js,*.jsx,*.mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.graphql,*.md,*.vue,*.yaml,*.html
                    "\ PrettierAsync
        "au BufWritePre *.js,*.jsx,*.ts,*.tsx
                    "\ PrettierAsync
    aug END
endif
