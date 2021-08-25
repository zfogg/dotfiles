" plugin/rc/neomake
scriptencoding utf-8


if z#util#HasPlugin('neomake')
    let g:neomake_open_list                 = 1
    let g:neomake_list_height               = 7
    let g:neomake_verbose                   = 0
    let g:neomake_airline                   = 0
    "let g:neomake_remove_invalid_entries    = 0
    let g:neomake_place_signs               = 0
    let g:neomake_highlight_columns         = 1
    let g:neomake_highlight_lines           = 1
    let g:neomake_virtualtext_current_error = 1
    let g:neomake_echo_current_error        = 1

    "call neomake#config#set('maker_defaults.remove_invalid_entries', 1)

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
            let s:jsbins = []
            let s:tsbins = []
            if executable('tsc')
                call add(s:jsbins, 'tsc')
                call add(s:tsbins, 'tsc')
                au FileType typescript let b:neomake_typescript_tsc_exe    = nrun#Which('tsc')
                au FileType javascript let b:neomake_javascript_tsc_exe    = nrun#Which('tsc')
            endif
            if executable('eslint_d')
                call add(s:jsbins, 'eslint')
                call add(s:tsbins, 'eslint')
                au FileType javascript let b:neomake_javascript_eslint_exe = nrun#Which('eslint_d')
                au FileType typescript let b:neomake_typescript_eslint_exe = nrun#Which('eslint_d')
            elseif executable('eslint')
                call add(s:jsbins, 'eslint')
                call add(s:tsbins, 'eslint')
                au FileType javascript let b:neomake_javascript_eslint_exe = nrun#Which('eslint')
                au FileType typescript let b:neomake_typescript_eslint_exe = nrun#Which('eslint')
            endif
            au FileType javascript let b:neomake_javasrcipt_enabled_makers = s:jsbins
            au FileType typescript let b:neomake_typescript_enabled_makers = s:tsbins
            "let g:neomake_enabled_javasript_makers  = s:lint_bins
            "let g:neomake_enabled_typescript_makers = s:lint_bins
        endif
    aug END
endif
