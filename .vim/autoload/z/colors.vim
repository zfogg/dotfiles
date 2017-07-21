" autoload/z/colors
scriptencoding utf-8


func! z#colors#Init() abort
    call z#colors#Guicolors()
    call z#colors#Colorsc()
endfunc


func! z#colors#Guicolors() abort
    if has('termguicolors') && has('nvim')
        set termguicolors
        set guicursor=n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50
                    \,a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor
                    \,sm:block-blinkwait175-blinkoff150-blinkon175
    endif
endfunc


func! z#colors#Colorsc() abort
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
endfunc
