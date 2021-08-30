" plugin/rc/nvim-lsp-installer
scriptencoding utf-8



if !PHas('nvim-lsp-installer') | finish | endif

lua << EOF
local lsp_installer = require'nvim-lsp-installer'
local lsp           = require'lspconfig'
--local configs       = require'lspconfig/configs'
local util          = require'lspconfig/util'

vim.api.nvim_command(':source ~/.vim/after/plugin/rc/nvim-lsp-ts-utils.vim')
require('lspconfig')['null-ls'].setup{}

--require('lang.keymappings')

function common_on_attach(client, bufnr)
  -- ... set up buffer keymaps, etc.

  if 1 == vim.fn.PHas('lsp_signature.nvim') then
    require "lsp_signature".on_attach({
      bind = true, -- This is mandatory, otherwise border config won't get registered.
                  -- If you want to hook lspsaga or other signature handler, pls set to false
      doc_lines = 2, -- will show two lines of comment/doc(if there are more than two lines in doc, will be truncated);
                    -- set to 0 if you DO NOT want any API comments be shown
                    -- This setting only take effect in insert mode, it does not affect signature help in normal
                    -- mode, 10 by default

      floating_window = true, -- show hint in a floating window, set to false for virtual text only mode
      fix_pos = false,  -- set to true, the floating window will not auto-close until finish all parameters
      hint_enable = true, -- virtual hint enable
      hint_prefix = "🐼 ",  -- Panda for parameter
      hint_scheme = "String",
      use_lspsaga = false,  -- set to true if you want to use lspsaga popup
      hi_parameter = "Search", -- how your parameter will be highlight
      max_height = 12, -- max height of signature floating_window, if content is more than max_height, you can scroll down
                      -- to view the hiding contents
      max_width = 120,  -- max_width of signature floating_window, line will be wrapped if exceed max_width
      handler_opts = {
        border = "single",   -- double, single, shadow, none
      },

      trigger_on_newline = false, -- set to true if you need multiple line parameter, sometime show signature on new line can be confusing, set it to false for #58
      extra_trigger_chars = {}, -- Array of extra characters that will trigger signature completion, e.g., {"(", ","}
      -- deprecate !!
      -- decorator = {"`", "`"}  -- this is no longer needed as nvim give me a handler and it allow me to highlight active parameter in floating_window
      zindex = 200, -- by default it will be on top of all floating windows, set to 50 send it to bottom
      debug = false, -- set to true to enable debug logging
      log_path = "debug_log_file_path", -- debug log path

      padding = '', -- character to pad on left and right of signature can be ' ', or '|'  etc

      shadow_blend = 36, -- if you using shadow as border use this set the opacity
      shadow_guibg = 'Black', -- if you using shadow as border use this set the color e.g. 'Green' or '#121315'
      toggle_key = nil, -- toggle signature on and off in insert mode,  e.g. toggle_key = '<M-x>'
      handler_opts = {
        border = "single",
      }
    }, bufnr)
  end


  local function buf_set_keymap(...)
    vim.api.nvim_buf_set_keymap(bufnr, ...)
  end
  local function buf_set_option(...)
    vim.api.nvim_buf_set_option(bufnr, ...)
  end

  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = {noremap = true, silent = true}
  buf_set_keymap('n', 'gD',          '<Cmd>lua vim.lsp.buf.declaration()<CR>',                                opts)
  buf_set_keymap('n', 'gd',          '<Cmd>lua vim.lsp.buf.definition()<CR>',                                 opts)
  buf_set_keymap('n', 'K',           '<Cmd>lua vim.lsp.buf.hover()<CR>',                                      opts)
  buf_set_keymap('n', 'gi',          '<cmd>lua vim.lsp.buf.implementation()<CR>',                             opts)
  buf_set_keymap('n', '<C-S-k>',     '<cmd>lua vim.lsp.buf.signature_help()<CR>',                             opts)
  buf_set_keymap('n', '[d',          '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>',                           opts)
  buf_set_keymap('n', ']d',          '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>',                           opts)
  buf_set_keymap('n', '<leader>law', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>',                       opts)
  buf_set_keymap('n', '<leader>lrw', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>',                    opts)
  buf_set_keymap('n', '<leader>llw', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<leader>lt',  '<cmd>lua vim.lsp.buf.type_definition()<CR>',                            opts)
  buf_set_keymap('n', '<leader>lrn', '<cmd>lua vim.lsp.buf.rename()<CR>',                                     opts)
  buf_set_keymap('n', '<leader>lrf', '<cmd>lua vim.lsp.buf.references()<CR>',                                 opts)
  buf_set_keymap('n', '<leader>ld',  '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>',               opts)
  buf_set_keymap('n', '<leader>ll',  '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>',                         opts)
  buf_set_keymap('n', '<leader>lca', '<cmd>lua vim.lsp.buf.code_action()<CR>',                                opts)

  -- Set some keybinds conditional on server capabilities
  if client.resolved_capabilities.document_formatting then
    buf_set_keymap("n", "<leader>lf",
    "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
  elseif client.resolved_capabilities.document_range_formatting then
    buf_set_keymap("n", "<leader>lf",
    "<cmd>lua vim.lsp.buf.range_formatting()<CR>", opts)
  end

  -- Set autocommands conditional on server_capabilities
  if client.resolved_capabilities.document_highlight then
    vim.api.nvim_exec([[
      hi LspReferenceRead cterm=bold ctermbg=red guibg=LightYellow
      hi LspReferenceText cterm=bold ctermbg=red guibg=LightYellow
      hi LspReferenceWrite cterm=bold ctermbg=red guibg=LightYellow
      augroup lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
    ]], false)
  end
end


local capabilities = vim.lsp.protocol.make_client_capabilities()
-- Code actions
capabilities.textDocument.codeAction = {
    dynamicRegistration = true,
    codeActionLiteralSupport = {
        codeActionKind = {
            valueSet = (function()
                local res = vim.tbl_values(vim.lsp.protocol.CodeActionKind)
                table.sort(res)
                return res
            end)()
        }
    }
}

capabilities.textDocument.completion.completionItem.documentationFormat = { 'markdown', 'plaintext' }
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.completion.completionItem.preselectSupport = true
capabilities.textDocument.completion.completionItem.insertReplaceSupport = true
capabilities.textDocument.completion.completionItem.labelDetailsSupport = true
capabilities.textDocument.completion.completionItem.deprecatedSupport = true
capabilities.textDocument.completion.completionItem.commitCharactersSupport = true
capabilities.textDocument.completion.completionItem.tagSupport = { valueSet = { 1 } }
capabilities.textDocument.completion.completionItem.resolveSupport = {
  properties = {
    'documentation',
    'detail',
    'additionalTextEdits',
  },
}

lsp_installer.on_server_ready(function(server)
  local opts = {
    on_attach = common_on_attach,
    flags = {
      debounce_text_changes = 500,
    },
    capabilities = capabilities,
  };

  if server.name == 'eslintls' then
    --local eslint = {
    --  lintCommand = "eslint_d -f unix --stdin --stdin-filename ${INPUT}",
    --  lintStdin = true,
    --  lintFormats = {"%f:%l:%c: %m"},
    --  lintIgnoreExitCode = true,
    --  formatCommand = "eslint_d --fix-to-stdout --stdin --stdin-filename=${INPUT}",
    --  formatStdin = true,
    --}
    opts.default_config = {
      cmd = {'eslint-ls', '--stdio'};
    }
    opts.settings = {
      format = { enable = true };
    };
    opts.filetypes = {
      'javascript',
      'javascriptreact',
      'javascript.jsx',
      'typescript',
      'typescriptreact',
      'typescript.tsx'
    };
    opts.root_dir = util.root_pattern(
      '.eslintrc',
      '.eslintrc.js',
      '.eslintrc.json',
      '.eslintrc.yaml',
      '.eslintignore',
      'package.json',
      '.git'
    );
  elseif server.name == 'sumneko_lua' then
    opts.settings = {
      Lua = {
        runtime = { version = "LuaJIT", path = vim.split(package.path, ';'), },
        diagnostics = { enable = true, globals = { "vim" }, }
      }
    }
  elseif server.name == 'rls' then
    opts.settings = {
      rust = {
        unstable_features = true,
        build_on_save = false,
        all_features = true,
      },
    }
  elseif server.name == 'tsserver' then
    function tsserver_on_attach(client, bufnr)
      if 1 == vim.fn.PHas('nvim-lsp-ts-utils') then
        -- disable tsserver formatting if you plan on formatting via null-ls
        client.resolved_capabilities.document_formatting = false
        client.resolved_capabilities.document_range_formatting = false

        local ts_utils = require('nvim-lsp-ts-utils')

        -- defaults
        ts_utils.setup{
          debug = false,
          disable_commands = false,
          enable_import_on_completion = true,
        -- import all
          import_all_timeout = 5000, -- ms
          import_all_priorities = {
            buffers = 4, -- loaded buffer names
            buffer_content = 3, -- loaded buffer content
            local_files = 2, -- git files or files with relative path markers
            same_file = 1, -- add to existing import statement
          },
          import_all_scan_buffers = 100,
          import_all_select_source = false,
          -- eslint
          eslint_enable_code_actions = true,
          eslint_enable_disable_comments = true,
          eslint_bin = 'eslint_d', -- INFO: OR 'eslint'
          eslint_config_fallback = os.getenv('HOME')..'/.dotfiles/.eslintrc.json',
          eslint_disable_if_no_config = true,
          eslint_enable_diagnostics = true,
          eslint_show_rule_id = true,
          -- formatting
          enable_formatting = false,
          formatter = 'prettier',
          formatter_config_fallback = nil,
          -- update imports on file move
          update_imports_on_move = false,
          require_confirmation_on_move = false,
          watch_dir = nil,
          -- filter diagnostics
          filter_out_diagnostics_by_severity = {},
          filter_out_diagnostics_by_code = {},
        }

        -- required to fix code action ranges and filter diagnostics
        ts_utils.setup_client(client)

        -- no default maps, so you may want to define some here
        local tslsp_opts = { silent = true }
        vim.api.nvim_buf_set_keymap(bufnr, "n", "gs", ":TSLspOrganize<CR>",   tslsp_opts)
        vim.api.nvim_buf_set_keymap(bufnr, "n", "qq", ":TSLspFixCurrent<CR>", tslsp_opts)
        vim.api.nvim_buf_set_keymap(bufnr, "n", "gr", ":TSLspRenameFile<CR>", tslsp_opts)
        vim.api.nvim_buf_set_keymap(bufnr, "n", "gi", ":TSLspImportAll<CR>",  tslsp_opts)
      end
      common_on_attach(client, bufnr)
    end
    opts.on_attach = tsserver_on_attach
  end

  server:setup(opts)

  if 1 == vim.fn.PHas('coq_nvim') and not server.name == 'eslintls' then
    server:setup(require('coq').lsp_ensure_capabilities(opts))
  end
    

  vim.cmd [[ do User LspAttachBuffers ]]
end)

if 1 == vim.fn.PHas('lspkind-nvim') then
  require('lspkind').init({
    -- enables text annotations
    --
    -- default: true
    with_text = true,
    --with_text = false,
    -- default symbol map
    -- can be either 'default' (requires nerd-fonts font) or
    -- 'codicons' for codicon preset (requires vscode-codicons font)
    --
    -- default: 'default'
    preset = 'default',
    -- override preset symbols
    --
    -- default: {}
    --symbol_map = {
    --  Text = "",
    --  Method = "",
    --  Function = "",
    --  Constructor = "",
    --  Field = "ﰠ",
    --  Variable = "",
    --  Class = "ﴯ",
    --  Interface = "",
    --  Module = "",
    --  Property = "ﰠ",
    --  Unit = "塞",
    --  Value = "",
    --  Enum = "",
    --  Keyword = "",
    --  Snippet = "",
    --  Color = "",
    --  File = "",
    --  Reference = "",
    --  Folder = "",
    --  EnumMember = "",
    --  Constant = "",
    --  Struct = "פּ",
    --  Event = "",
    --  Operator = "",
    --  TypeParameter = "",
    --},
  })
end

-- symbols-outline.nvim
vim.g.symbols_outline = {
    highlight_hovered_item = true,
    show_guides = true,
    auto_preview = false, -- experimental
    position = 'right',
    keymaps = {
        close = "<Esc>",
        goto_location = "<Cr>",
        focus_location = "o",
        hover_symbol = "<C-space>",
        rename_symbol = "r",
        code_actions = "a"
    },
    lsp_blacklist = {}
}

-- LSP Enable diagnostics
vim.lsp.handlers["textDocument/publishDiagnostics"] =
    vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
        virtual_text = false,
        underline = true,
        signs = true,
        update_in_insert = false
    })

-- Send diagnostics to quickfix list
do
    local method = "textDocument/publishDiagnostics"
    local default_handler = vim.lsp.handlers[method]
    vim.lsp.handlers[method] = function(err, method, result, client_id, bufnr,
                                        config)
        default_handler(err, method, result, client_id, bufnr, config)
        local diagnostics = vim.lsp.diagnostic.get_all()
        local qflist = {}
        for bufnr, diagnostic in pairs(diagnostics) do
            for _, d in ipairs(diagnostic) do
                d.bufnr = bufnr
                d.lnum = d.range.start.line + 1
                d.col = d.range.start.character + 1
                d.text = d.message
                table.insert(qflist, d)
            end
        end
        vim.lsp.util.set_qflist(qflist)
    end
end

vim.o.shortmess = vim.o.shortmess .. "c"

EOF
