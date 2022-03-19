" ftdetect/typescript.tsx
scriptencoding utf-8


" INFO: now enabled for all typescript files.. questionable at best
"\ *.tsx,*.jsx
"\ *.ts,*.tsx,*.jsx
"autocmd BufRead,BufNewFile *.jsx set filetype=javascript.jsx
"autocmd BufRead,BufNewFile *.tsx set filetype=typescript.tsx

" INFO: https://github.com/peitalin/vim-jsx-typescript
autocmd BufNewFile,BufRead *.tsx,*.jsx set filetype=typescriptreact
