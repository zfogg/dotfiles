" plugin/rc/languageclient
scriptencoding utf-8


if !PHas('LanguageClient-neovim') | finish | endif


let g:LanguageClient_showCompletionDocs = 1
let g:LanguageClient_autoStart = 1
"let g:LanguageClient_loggingLevel = 'INFO'
let g:LanguageClient_selectionUI_autoOpen = 0
"let g:LanguageClient_loggingFile  = expand('~/lc.log')
"let g:LanguageClient_serverStderr = expand('~/lc.err')

let g:LanguageClient_useFloatingHover = 0

finish " FIXME

let g:LanguageClient_serverCommands = get(g:, 'LanguageClient_serverCommands', {})


let g:LanguageClient_serverCommands['rust'] = [ 'rustup', 'run', 'stable', 'rls', ]

"let g:LanguageClient_serverCommands['javascript']      = ['vscode-eslint-language-server', '--stdio', ]
let g:LanguageClient_serverCommands['javascript']      = ['typescript-language-server', '--stdio', ]
" choose ONE to uncomment ^
let g:LanguageClient_serverCommands['typescript']      = g:LanguageClient_serverCommands['javascript']
let g:LanguageClient_serverCommands['javascript.jsx']  = g:LanguageClient_serverCommands['javascript']
let g:LanguageClient_serverCommands['javascriptreact'] = g:LanguageClient_serverCommands['javascript']
let g:LanguageClient_serverCommands['typescript.tsx']  = g:LanguageClient_serverCommands['javascript']
let g:LanguageClient_serverCommands['typescriptreact'] = g:LanguageClient_serverCommands['javascript']


let g:LanguageClient_serverCommands['vim'] = ['vim-language-server', '--stdio', ]
"finish " FIXME

"let g:LanguageClient_serverCommands['go'] = ['gopls', ]

let g:LanguageClient_serverCommands['python'] = ['pyls', ]

let g:LanguageClient_serverCommands['solidity'] = ['solidity-language-server', '--stdio', ]

let g:LanguageClient_serverCommands['bash'] = ['bash-language-server', 'start', ]
let g:LanguageClient_serverCommands['sh']   = ['bash-language-server', 'start', ]

let g:LanguageClient_serverCommands['lua']   = ['lua-language-server', ]

let g:LanguageClient_serverCommands['css']    = ['vscode-css-language-server', '--stdio', ]
let g:LanguageClient_serverCommands['scss']   = ['vscode-css-language-server', '--stdio', ]
let g:LanguageClient_serverCommands['less']   = ['vscode-css-language-server', '--stdio', ]

let g:LanguageClient_serverCommands['json']   = ['vscode-json-language-server', '--stdio', ]
let g:LanguageClient_serverCommands['yaml']   = ['yaml-language-server', '--stdio', ]
let g:LanguageClient_serverCommands['yaml.docker-compose']   = g:LanguageClient_serverCommands['yaml']

let g:LanguageClient_serverCommands['html']   = ['vscode-html-language-server', '--stdio', ]

let g:LanguageClient_serverCommands['dockerfile']   = ['docker-langserver', '--stdio', ]

let g:LanguageClient_serverCommands['dot']   = ['dot-language-server', '--stdio', ]
