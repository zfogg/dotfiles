-- rc/nvim-scrollview

if vim.fn.has('nvim') == 0 then return end
if not _G.PHas('nvim-scrollview') then return end

pcall(vim.cmd, 'ScrollViewEnable')
