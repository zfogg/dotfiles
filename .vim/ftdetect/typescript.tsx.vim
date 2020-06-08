" ftdetect/typescript.tsx
scriptencoding utf-8


" INFO: now enabled for all typescript files.. questionable at best
"\ *.tsx,*.jsx
"\ *.ts,*.tsx,*.jsx
au BufRead,BufNewFile
        \ *.ts,*.tsx,*.jsx
    \ set filetype=typescript.tsx

" FIXME: hack!
au BufReadPost
        \ *.ts
    \ set filetype=typescript
