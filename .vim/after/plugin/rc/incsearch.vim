" incsearch
scriptencoding utf-8


"finish " FIXME: not using this plugin
if (!exists('g:incsearch')) | finish | endif


"set hlsearch
let g:incsearch#auto_nohlsearch                   = 0
let g:incsearch#consistent_n_direction            = 1
let g:incsearch#do_not_save_error_message_history = 1


map /          <Plug>(incsearch-forward)
map ?          <Plug>(incsearch-backward)
map g/         <Plug>(incsearch-stay)

map <C-f>      <Plug>(incsearch-fuzzy-/)
map <S-f>      <Plug>(incsearch-fuzzy-?)
map g<C-f>     <Plug>(incsearch-fuzzy-stay)

map <Leader>/  <Plug>(incsearch-easymotion-forward)
map <Leader>?  <Plug>(incsearch-easymotion-backward)
map <Leader>g/ <Plug>(incsearch-easymotion-stay)


map n          <Plug>(incsearch-nohl-n)
map N          <Plug>(incsearch-nohl-N)

map *          <Plug>(incsearch-nohl-*)
map #          <Plug>(incsearch-nohl-#)

map g*         <Plug>(incsearch-nohl-g*)
map g#         <Plug>(incsearch-nohl-g#)
