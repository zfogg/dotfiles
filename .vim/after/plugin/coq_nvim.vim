" plugin/rc/coq_nvim
scriptencoding utf-8


if !has('nvim') | finish | endif
"if !PHas('coq_nvim') | finish | endif
"if !PHas('nvim-lspconfig') | finish | endif
"if !PHas('nvim-lspinstall') | finish | endif
"if !PHas('nvim-lsp-installer') | finish | endif


let g:coq_settings = get(g:, 'coq_settings', {})

let g:coq_settings.auto_start = 'shut-up'

let g:coq_settings.keymap = {
    \ 'recommended':     v:true,
    \ 'manual_complete': '<C-Space>',
    \ 'bigger_preview':  '<C-S-i>',
    \ 'jump_to_mark':    '<C-S-y>',
    \ }

let g:coq_settings.clients = {
    \ 'buffers': { 'match_syms':    v:true, },
    \ 'tmux':    { 'match_syms':    v:false, },
    \ 'paths':   { 'preview_lines': 8, },
    \ }
