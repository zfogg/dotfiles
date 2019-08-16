"  vim: filetype=vim:
scriptencoding utf-8


" INFO: https://stackoverflow.com/a/30552423/672346
aug vimrc_syn_todo
    au!
    au Syntax * syn match MyTodo /\v<(FIXME|NOTE|INFO|TODO|BUG):/
          \ containedin=.*Comment,vimCommentTitle
aug END

hi def link MyTodo Todo
