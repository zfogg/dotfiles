" plugin/rc/repeat
scriptencoding utf-8


if z#util#HasPlugin('vim-repeat')
    runtime autoload/repeat.vim
    nnoremap <silent> <Plug>(RepeatRedo) :<C-U>call repeat#wrap("\\<Lt>C-R>",v:count)<CR>
endif
