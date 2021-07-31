" plugin/rc/neomake
scriptencoding utf-8


if z#util#HasPlugin('neomake')
    let g:neomake_open_list              = 0
    let g:neomake_list_height            = 5
    let g:neomake_verbose                = 0
    let g:neomake_airline                = 0
    let g:neomake_remove_invalid_entries = 1
    let g:neomake_place_signs            = 1
    let g:neomake_highlight_columns      = 1
    let g:neomake_highlight_lines        = 1

    try
        let s:neomake_tempfile_dir = substitute(z#util#TempDirs('/neomake/'), ',.*$', '', v:null)
        if !isdirectory(s:neomake_tempfile_dir)
            call mkdir(s:neomake_tempfile_dir, 'p')
        endif
        let g:neomake_tempfile_dir = s:neomake_tempfile_dir . '%:p:h'
    catch
        "silent! unlet g:neomake_tempfile_dir
        echom expand('<sfile>')
        echom v:errmsg
    endtry


    hi NeomakeErrorSign   term=bold gui=bold ctermfg=red    guifg=red
    hi NeomakeWarningSign term=bold gui=bold ctermfg=yellow guifg=yellow

    aug RcPlugin__neomake
        au!
        if z#util#HasPlugin('nrun.vim')
            if executable('eslint_d')
                au FileType javascript let b:neomake_javascript_eslint_exe = nrun#Which('eslint_d')
                au FileType typescript let b:neomake_typescript_eslint_exe = nrun#Which('eslint_d')
            elseif executable('eslint')
                au FileType javascript let b:neomake_javascript_eslint_exe = nrun#Which('eslint')
                au FileType typescript let b:neomake_typescript_eslint_exe = nrun#Which('eslint')
            endif
            if executable('tsc')
                au FileType typescript let b:neomake_typescript_tsc_exe    = nrun#Which('tsc')
            endif
        endif
    aug END
endif
