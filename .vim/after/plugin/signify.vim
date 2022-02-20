" plugin/rc/signify
scriptencoding utf-8


if !PHas('vim-signify') | finish | endif


let g:signify_vcs_list = [
        \ 'git'
    \ ]


nnoremap <Leader>gg<Space>  :SignifyToggle<CR>
nnoremap <Leader>ggl<Space> :SignifyToggleHighlight<CR>

nnoremap <Leader>ggd        :SignifyDiffPreview<CR>
nnoremap <Leader>ggD        :SignifyDiff!<CR>

nmap == <plug>(signify-next-hunk)
nmap -- <plug>(signify-prev-hunk)


aug RcPlugin__signify
    au!
    au ColorScheme,VimEnter * highlight link SignifyLineAdd DiffAdded
    au ColorScheme,VimEnter * highlight SignifyLineAdd               ctermbg=18               guibg=#282a2e gui=bold
    au ColorScheme,VimEnter * highlight SignifyLineChange            ctermbg=18               guibg=#28322d gui=bold,italic,underline
    au ColorScheme,VimEnter * highlight SignifyLineDelete ctermfg=7  ctermbg=0  guifg=#773333 guibg=#e0e0e0 gui=italic,standout
    au ColorScheme,VimEnter * highlight link SignifyLineDelete DiffDelete

    au ColorScheme,VimEnter * highlight DiffAdd           cterm=bold ctermbg=none ctermfg=119
    au ColorScheme,VimEnter * highlight DiffDelete        cterm=bold ctermbg=none ctermfg=167
    au ColorScheme,VimEnter * highlight DiffChange        cterm=bold ctermbg=none ctermfg=227
    au ColorScheme,VimEnter * highlight SignifySignAdd    cterm=bold ctermbg=237  ctermfg=119
    au ColorScheme,VimEnter * highlight SignifySignDelete cterm=bold ctermbg=237  ctermfg=167
    au ColorScheme,VimEnter * highlight SignifySignChange cterm=bold ctermbg=237  ctermfg=227
    au ColorScheme,VimEnter * highlight SignColumn        cterm=NONE ctermbg=NONE guibg=NONE gui=NONE
aug END
