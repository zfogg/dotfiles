" session
scriptencoding utf-8


if !PHas('vim-session') | finish | endif

let g:session_default_name      = 'default'           " When you don't name your session.
let g:session_command_aliases   = 1                   " Use commands prefixed with 'Session'.
let g:session_autosave          = 'yes'               " Save on exit sans prompt.
let g:session_autosave_periodic = 2                   " Save every 'n' minutes.
let g:session_autoload          = 0                   " Don't ask to open default session.
let g:session_verbose_messages  = 0                   " Disable noisy save messages.
let g:session_persist_globals   = ['&sessionoptions'] " Persist `vim-session` plugin opts


call add(g:session_persist_globals, 'g:session_autosave')
call add(g:session_persist_globals, 'g:session_autosave_periodic')
call add(g:session_persist_globals, 'g:session_autoload')
call add(g:session_persist_globals, 'g:session_default_to_last')
call add(g:session_persist_globals, 'g:session_persist_globals')


nnoremap <Leader>So :OpenSession<CR>
nnoremap <Leader>Ss :SaveSession<CR>
nnoremap <Leader>Sq :SaveSession<CR>:CloseSession<CR>
nnoremap <Leader>SQ <Leader>Sq:q<CR>
