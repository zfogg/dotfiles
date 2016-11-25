" rc/keys_vimpager
scriptencoding utf-8


let g:mapleader=','

inoremap <C-c> <Esc>

noremap : <Nop>
noremap ; :


unmap v

unmap y


call z#keys#Tmux()

nnoremap j gj
nnoremap k gk


" Previous matching command or search.
cnoremap kk <Up>
cnoremap HH <Home>
cnoremap LL <Home>
