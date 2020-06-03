" plugin/rc/csv.vim
scriptencoding utf-8


" INFO: https://github.com/chrisbra/csv.vim
if z#util#HasPlugin('csv.vim')
    let g:csv_autocmd_arrange = 1
    let g:csv_autocmd_arrange_size = 1024*1024
endif
