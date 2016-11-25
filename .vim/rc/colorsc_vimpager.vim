" rc/colorsc
scriptencoding utf-8


if has('termguicolors')
    set termguicolors
    let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif


try
    let g:base16colorspace  = 256
    let g:base16_shell_path = $BASE16_SHELL.'/scripts'
    if filereadable(expand('~/.vimrc_background'))
        source ~/.vimrc_background
    endif

catch
    try
        colorscheme jellybeans
    catch /^Vim(colorscheme):E185/
        colorscheme desert
    endtry
endtry
