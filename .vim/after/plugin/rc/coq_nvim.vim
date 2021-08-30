" plugin/rc/coq_nvim
scriptencoding utf-8


if !has('nvim') | finish | endif
if !PHas('coq_nvim') | finish | endif
if !PHas('nvim-lspconfig') | finish | endif
"if !PHas('nvim-lspinstall') | finish | endif
if !PHas('nvim-lsp-installer') | finish | endif

let g:coq_settings = get(g:, 'coq_settings', {
    \ 'auto_start': 'shut-up',
    \ 'clients': {
      \ 'buffers': { 'match_syms':    v:true, },
      \ 'tmux':    { 'match_syms':    v:false, },
      \ 'paths':   { 'preview_lines': 8, },
      \ },
      \ 'keymap': {
        \ 'recommended':     v:true,
        \ 'manual_complete': '<C-Space>',
        \ 'bigger_preview':  '<C-S-i>',
        \ 'jump_to_mark':    '<C-S-y>',
        \ },
    \ })


" FIXME
finish

lua << EOF
--local coq  = require'coq'
--local lsp  = require'lspconfig'
--local lspi    = require'lspinstall'
--local servers = lspi.installed_servers()
--local lspi    = require'nvim-lsp-installer'
--local servers = lspi.get_installed_servers()

--vim.schedule(function()
--  for _,s in pairs(servers) do
--    lsp[s.name].setup(coq.lsp_ensure_capabilities{})
--  end
--end)
EOF
