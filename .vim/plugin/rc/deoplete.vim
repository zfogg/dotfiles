" plugin/rc/deoplete
scriptencoding utf-8


if !has('nvim') | finish | endif
if !PHas('deoplete.nvim') | finish | endif
if !PHas('deoplete-clang') | finish | endif

let g:deoplete#enable_at_startup = 1  " just work

call deoplete#custom#option('smart_case', v:true)
call deoplete#custom#option('max_list', 65)
call deoplete#custom#option('min_pattern_length', 1)

call deoplete#custom#source('_', 'max_menu_width', 70)

if PHas('deoplete-tabnine')
    call deoplete#custom#var('tabnine', {
        \ 'line_limit': 750,
        \ 'max_num_results': 6,
    \ })
endif

"let g:deoplete#enable_smart_case = 1  " smartcase
"let g:deoplete#max_list          = 65 " default=100
"let g:deoplete#max_menu_width    = 85

if has('mac')
    let g:deoplete#sources#clang#libclang_path = '/Library/Developer/CommandLineTools/usr/lib/libclang.dylib'
    let g:deoplete#sources#clang#flags = [
                \ "-cc1",
                \ "-triple", "x86_64-apple-macosx10.15.0",
                \ "-isysroot", "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/",
                \ "-fmax-type-align=16",
                \ ]
    let g:deoplete#sources#clang#clang_header  = $BREW."/opt/llvm/lib/clang"
elseif has('unix')
    let g:deoplete#sources#clang#libclang_path = '/usr/local/libclang.so'
    let g:deoplete#sources#clang#flags = [
                \ "-cc1",
                \ "-triple", "x86_64-pc-linux-gnu",
                \ "-fmax-type-align=16",
                \ ]
elseif has('win32')
endif

let g:deoplete#sources#clang#std                       = {'c': 'c11', 'cpp': 'c++1z', 'objc': 'c11', 'objcpp': 'c++1z'}
let g:deoplete#sources#clang#sort_algo                 = ''
"let g:deoplete#sources#clang#clang_complete_database   = ''
let g:deoplete#sources#clang#include_default_arguments = v:true
let g:deoplete#sources#clang#filter_availability_kinds = ['NotAvailable', 'NotAccessible']
