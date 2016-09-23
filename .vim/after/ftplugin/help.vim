" ftplugin/help
scriptencoding utf-8

if exists('b:did_load_filetypes_userafter')
  finish
endif
let b:did_load_filetypes_userafter = 1


nnoremap <buffer> <Leader>w <Nop>
nnoremap <buffer> <Leader>W <Nop>

"wincmd L | wincmd h | wincmd L | wincmd p


let b:noNERDTreeAutoCWD=1
