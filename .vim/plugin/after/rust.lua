-- rc/rust

vim.g.rust_bang_comment_leader = 1
vim.g.rust_fold = 2
vim.g.rustfmt_autosave = 1
vim.g.rustfmt_fail_silently = 1
vim.g.ftplugin_rust_source_path = vim.fn.expand(vim.env.RUST_SRC_PATH or '')
