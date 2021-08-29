" plugin/rc/rust
scriptencoding utf-8


let g:rust_bang_comment_leader  = 1
let g:rust_fold                 = 2
let g:rustfmt_autosave          = 1
let g:rustfmt_fail_silently     = 1
let g:ftplugin_rust_source_path = expand($RUST_SRC_PATH)

let g:LanguageClient_autoStart = 1
let g:LanguageClient_serverCommands = get(g:, 'LanguageClient_serverCommands', {})
let g:LanguageClient_serverCommands['rust'] = [
    \ 'rustup', 'run', 'stable', 'rls']
