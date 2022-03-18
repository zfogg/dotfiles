" after/plugin/fern
scriptencoding utf-8


if !PHas('fern.vim') | finish | endif

nnoremap <Leader>n<Space>     :Fern . -drawer       -reveal=% -toggle<CR>
nnoremap <Leader>nn           :Fern . -drawer -wait -reveal=%<BAR>wincmd p<CR>

if PHas('fern-renderer-nerdfont.vim')
  let g:fern#renderer = "nerdfont"
endif

let g:fern#disable_viewer_auto_duplication=1

" INFO: https://github.com/lambdalisue/fern.vim

augroup glyph-palette-custom
  autocmd! *
  autocmd FileType fern call glyph_palette#apply()
  autocmd FileType nerdtree,startify call glyph_palette#apply()
augroup END


"let g:fern_custom_isFloating = v:false
"function! Fern_ShowFloatingPreview()
  "if g:fern_custom_isFloating == v:false
    "let g:fern_custom_isFloating = v:true
    "call fern#action#call('preview:auto:toggle')
  "endif
"endfunction
"function! Fern_ToggleFloatingPreview()
  "call fern#action#call('preview:toggle')
"endfunction
"function! Fern_AutoToggleFloatingPreview()
  "if g:fern_custom_isFloating == v:true
    "let g:fern_custom_isFloating = v:false
  "else
    "let g:fern_custom_isFloating = v:true
  "endif
  "call fern#action#call('preview:auto:toggle')
"endfunction


function! s:init_fern() abort
  nmap <buffer> o  <Plug>(fern-action-open:edit)
  nmap <buffer> go <Plug>(fern-action-open:edit)<C-w>p
  nmap <buffer> t  <Plug>(fern-action-open:tabedit)
  nmap <buffer> T  <Plug>(fern-action-open:tabedit)gT
  nmap <buffer> i  <Plug>(fern-action-open:split)
  nmap <buffer> gi <Plug>(fern-action-open:split)<C-w>p
  nmap <buffer> s  <Plug>(fern-action-open:vsplit)
  nmap <buffer> gs <Plug>(fern-action-open:vsplit)<C-w>p
  nmap <buffer> ma <Plug>(fern-action-new-path)
  nmap <buffer> P  gg

  nmap <buffer> C      <Plug>(fern-action-enter)
  nmap <buffer> u      <Plug>(fern-action-leave)
  nmap <buffer> r      <Plug>(fern-action-reload)
  nmap <buffer> R    gg<Plug>(fern-action-reload)<C-o>
  nmap <buffer> cd     <Plug>(fern-action-cd)
  nmap <buffer> CD   gg<Plug>(fern-action-cd)<C-o>

  nmap <buffer> I <Plug>(fern-action-hidden-toggle)

  nmap <buffer> q :<C-u>quit<CR>

  " Use 'select' instead of 'edit' for default 'open' action
  nmap <buffer> <Plug>(fern-action-open) <Plug>(fern-action-open:select)
  " Let you move to a right split lol
  silent! unmap <buffer> <C-l>
  silent! unmap <buffer> <C-h>

  nmap <buffer><expr>
      \ <Plug>(fern-my-expand-or-collapse)
      \ fern#smart#leaf(
      \   "\<Plug>(fern-action-collapse)",
      \   "\<Plug>(fern-action-expand)",
      \   "\<Plug>(fern-action-collapse)",
      \ )

  "nmap <buffer><nowait> l <Plug>(fern-my-expand-or-collapse)

  nmap <buffer><expr>
      \ <Plug>(fern-my-expand-or-enter)
      \ fern#smart#drawer(
      \   "\<Plug>(fern-action-open-or-expand)",
      \   "\<Plug>(fern-action-open-or-enter)",
      \ )
  nmap <buffer><expr>
      \ <Plug>(fern-my-collapse-or-leave)
      \ fern#smart#drawer(
      \   "\<Plug>(fern-action-collapse)",
      \   "\<Plug>(fern-action-leave)",
      \ )
  nmap <buffer><nowait> l <Plug>(fern-my-expand-or-enter)
  nmap <buffer><nowait> h <Plug>(fern-my-collapse-or-leave)

  " INFO: https://github.com/lambdalisue/fern.vim/wiki/Tips
  nmap <buffer> <Plug>(fern-my-enter-and-tcd)
      \ <Plug>(fern-action-enter)
      \ <Plug>(fern-wait)
      \ <Plug>(fern-action-tcd:root)

  nmap <buffer> <Plug>(fern-my-leave-and-tcd)
      \ <Plug>(fern-action-leave)
      \ <Plug>(fern-wait)
      \ <Plug>(fern-action-tcd:root)

  "nmap <buffer><expr>
      "\ <Plug>(fern-floating-preview-or-nop)
      "\ fern#smart#leaf(
      "\   ":silent! call Fern_ShowFloatingPreview()\<CR>",
      "\   "",
      "\ )
  "nmap <buffer><expr>
      "\ <Plug>(fern-floating-preview-or-nop)
      "\ fern#smart#leaf(
      "\   "\<Plug>(fern-action-preview:toggle)",
      "\   "",
      "\ )

  "nmap <buffer><expr>
      "\ <Plug>(fern-my-preview-or-nop)
      "\ fern#smart#leaf(
      "\   "\<Plug>(fern-action-open:edit)\<C-w>p",
      "\   "",
      "\ )

  nmap <buffer><expr> j
      \ fern#smart#drawer(
      \   "j\<Plug>(fern-floating-preview-or-nop)",
      \   "j",
      \ )
  nmap <buffer><expr> k
      \ fern#smart#drawer(
      \   "k\<Plug>(fern-floating-preview-or-nop)",
      \   "k",
      \ )

  nmap <silent> <buffer> p     <Plug>(fern-action-preview:toggle)
  "nmap <silent> <buffer> p     :call Fern_ToggleFloatingPreview()<CR>
  nmap <silent> <buffer> <C-p> <Plug>(fern-action-preview:auto:toggle)
  "nmap <silent> <buffer> <C-p> :call Fern_AutoToggleFloatingPreview()<CR>
  nmap <silent> <buffer> <C-d> <Plug>(fern-action-preview:scroll:down:half)
  nmap <silent> <buffer> <C-u> <Plug>(fern-action-preview:scroll:up:half)

  nmap <silent> <buffer> <expr> <Plug>(fern-quit-or-close-preview) fern_preview#smart_preview("\<Plug>(fern-action-preview:close)", ":q\<CR>")
  nmap <silent> <buffer> q <Plug>(fern-quit-or-close-preview)
endfunction

augroup fern-custom
  autocmd! *
  autocmd FileType fern call s:init_fern()
  autocmd FileType fern call fzf#vim#with_preview()
augroup END


function! Fern_mapping_fzf_customize_option(spec)
    let a:spec.options .= ' --multi'
    " Note that fzf#vim#with_preview comes from fzf.vim
    if exists('*fzf#vim#with_preview')
        return fzf#vim#with_preview(a:spec)
    else
        return a:spec
    endif
endfunction

function! Fern_mapping_fzf_before_all(dict)
    if !len(a:dict.lines)
        return
    endif
    return a:dict.fern_helper.async.update_marks([])
endfunction

function! s:Fern_FZF_reveal(dict)
    execute 'FernReveal -wait' a:dict.relative_path
    execute 'normal \<Plug>(fern-action-mark:set)'
endfunction

let g:Fern_mapping_fzf_file_sink = function('s:Fern_FZF_reveal')
let g:Fern_mapping_fzf_dir_sink  = function('s:Fern_FZF_reveal')
