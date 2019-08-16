" rc/settings
scriptencoding utf-8


" NOTE: requires any 'powerline' font


aug RcPlugin__statusline_highlight
    au!
    "au VimEnter,Colorscheme * call z#statusline#Highlight()
aug END


set statusline= " {{{
" buffer number
set stl+=%(\ %{
    \'help'!=&ft?bufnr('%'):'[help]'
    \}\ %)

" filename
set stl+=\ %{(substitute(
    \expand('%:h:t').'/'.expand('%:t'),
    \'^\.\/','',''))}
set stl+=%(\ %)
if has('nvim')
    set stl+=%(%#ErrorMsg#%{
        \neomake#statusline#QflistStatus('qf\ :')
    \}%)
endif
set stl+=%(%1*%)

" META: center
set stl+=%=

" buffer
set stl+=%(\ %{
    \(&readonly?
        \'🔐':
        \(&modified?'🔴':'✅').'\ 📝\ ')
    \}\ %)

" whitespace
set stl+=%(\ %{
    \(&modifiable?
        \'↳\ '.(&et?&ts:'et'):
        \'')
    \}\ %)

" encoding and format
set stl+=%(\ %{
    \(&ft!=''?
        \WebDevIconsGetFileTypeSymbol():
        \'[none]').
    \(&bomb\|\|'^$\|utf-8'!~#&fenc?
        \&fenc.(&bomb?'-bom':''):
        \'').
    \('unix'!=#&ff?
        \('\ '.WebDevIconsGetFileFormatSymbol()):
        \'')
    \}\ %)

" cursor
set stl+=%(%*%3l\ %3p%%\ %)

" }}}
