" ftplugin/csv
scriptencoding utf-8


aug CSV_Editing
    au!
    au BufRead,BufWritePost <buffer> :%ArrangeColumn
    au BufWritePre          <buffer> :%UnArrangeColumn
aug END
