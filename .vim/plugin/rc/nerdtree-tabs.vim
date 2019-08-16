" nerdtree-tabs
scriptencoding utf-8


"finish


let g:nerdtree_tabs_open_on_gui_startup = 2
"Open NERDTree on gvim/macvim startup. (When set to 2, open only if directory was given as startup argument).

let g:nerdtree_tabs_open_on_console_startup = 2
"Open NERDTree on console vim startup. (When set to 2, open only if directory was given as startup argument).

let g:nerdtree_tabs_no_startup_for_diff = 1
"Do not open NERDTree if vim starts in diff mode

let g:nerdtree_tabs_smart_startup_focus = 1
"On startup, focus NERDTree if opening a directory, focus file if opening a file. (When set to 2, always focus file window after startup).

let g:nerdtree_tabs_open_on_new_tab = 1
"Open NERDTree on new tab creation (if NERDTree was globally opened by :NERDTreeTabsToggle)

let g:nerdtree_tabs_meaningful_tab_names = 1
"Unfocus NERDTree when leaving a tab for descriptive tab names

let g:nerdtree_tabs_autoclose = 1
"Close current tab if there is only one window in it and it's NERDTree

let g:nerdtree_tabs_synchronize_view = 0
"Synchronize view of all NERDTree windows (scroll and cursor position)

let g:nerdtree_tabs_synchronize_focus = 0
"Synchronize focus when switching windows (focus NERDTree after tab switch if and only if it was focused before tab switch)

let g:nerdtree_tabs_focus_on_files = 1
"When switching into a tab, make sure that focus is on the file window, not in the NERDTree window. (Note that this can get annoying if you use NERDTree's feature "open in new tab silently", as you will lose focus on the NERDTree.)

let g:nerdtree_tabs_startup_cd = 1
"When given a directory name as a command line parameter when launching Vim, :cd into it.

" NOTE: buggy af - @zfogg
let g:nerdtree_tabs_autofind = 0
"Automatically find and select currently opened file in NERDTree.
