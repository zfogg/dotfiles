" autoload/z/constants/globals
scriptencoding utf-8


func! z#constants#globals#Ft() abort
  let g:zft = {
      \'py':     ['python', 'python3'],
      \'js':     ['javascript', 'javascript.jsx'],
      \'cx':     ['c', 'cpp', 'objc', 'objcpp', 'ch'],
      \'jinja':  ['jinja', 'jinja.html', 'sls'],
      \'markup': ['xml', 'html', 'jinja.html'],
      \'styles': ['css', 'sass', 'scss', 'less', 'stylus'],
      \'jade':   ['jade', 'pug'],
      \'image':  ['jpg', 'jpeg', 'gif', 'png', 'ico'],
      \'config': ['toml', 'yaml', 'json', 'markdown', 'apiblueprint'],
      \'scpt':   ['applescript', 'osascript'],
      \'clj':    ['clojure', 'clojurescript'],
      \'sql':    ['sql', 'pgsql'],
      \'vim':    ['vim'],
      \'rs':     ['rust'],
      \'php':    ['php'],
      \'twig':   ['twig', 'html.twig'],
      \'stylus': ['stylus'],
      \'smali':  ['smali']
  \}
  return g:zft
endfunc

func! z#constants#globals#ConfigPaths() abort
    " g:mvvar =~# escape('s:\i+(_(l|f|r))?', '+()|?')
        " g:myvar_l  (l)ocal path basename
        " g:myvar_f  (f)ully resolved links (expanded vars)
        " g:myvar_r  (r)elative to $HOME (un-expanded vars)

    " dir/ ~/.dotfiles/ 'g:dotfiles'
    let g:dotfiles_l = '.dotfiles'
    let g:dotfiles   = fnamemodify(exists('$DOTFILES') ? $DOTFILES : g:dotfiles_l, ':~')
    let g:dotfiles_f = expand(g:dotfiles)
    let g:dotfiles_r = fnamemodify(g:dotfiles, ':~')

    " dir/ ~/.vim/ 'g:dotvim'
    let g:dotvim_l   = '.vim'
    let g:dotvim     = exists('$DOTVIM')               ? $DOTVIM   : g:dotfiles.'/'.g:dotvim_l
    let g:dotvim_f   = expand(g:dotvim)
    let g:dotvim_r   = fnamemodify(g:dotvim,   ':~')

    " file ~/.vim/init.vim 'g:myvimrc'
    let g:myvimrc_l = 'init.vim'
    let g:myvimrc   = g:dotvim  .'/'.g:myvimrc_l
    let g:myvimrc_f = g:dotvim_f.'/'.g:myvimrc_l
    let g:myvimrc_r = g:dotvim_r.'/'.g:myvimrc_l
endfunc


func! z#constants#globals#Python() abort
  if has('win32')
    let l:py3_root = $USERPROFILE.'/scoop/shims'
    let l:py_root  = $USERPROFILE.'/scoop/shims'
  elseif exists('$PYENV_ROOT')
    let l:py3_root = $PYENV_ROOT.'/versions/neovim3'
    let l:py_root  = $PYENV_ROOT.'/versions/neovim2'
  elseif exists('$BREW')
    let l:py3_root = $BREW
    let l:py_root  = $BREW
  else
    throw "error with python config :("
  endif
  let g:python3_host_prog = l:py3_root.'/bin/python3'
  let g:python_host_prog  = l:py_root .'/bin/python2'
endfunc


func! z#constants#globals#Nodejs() abort
  try
    if has('unix')
      let l:node_host = "neovim-node-host"
      let l:which_prog = systemlist("which ".l:node_host)[0]
      if v:shell_error != 0 | throw "Z:NotFound ".l:node_host | endif
      let l:host_path = fnamemodify(l:which_prog, ':p:h')
    elseif has('win32')
      let l:node_host = "neovim-node-host.cmd"
      let l:host_path = expand($USERPROFILE).'/scoop/apps/nodejs/current/bin'
    endif
    let g:node_host_prog = l:host_path.'/'.l:node_host
  finally
    if exists("g:node_host_prog") && !filereadable(g:node_host_prog)
      throw "Z:NotFound neovim-node-host ".g:node_host_prog
      unlet g:node_host_prog
    endif
  endtry
endfunc


func! z#constants#globals#Ruby() abort
  try
    if has('unix')
      let l:ruby_host = "neovim-ruby-host"
      let l:which_prog = systemlist("which ".l:ruby_host)[0]
      if v:shell_error != 0 | throw "Z:NotFound ".l:ruby_host | endif
      let l:host_path = fnamemodify(l:which_prog, ':p:h')
    elseif has('win32')
      let l:ruby_host = "neovim-ruby-host.cmd"
      let l:host_path = expand($USERPROFILE).'/scoop/apps/ruby/current/gems/bin'
    endif
    let g:ruby_host_prog = l:host_path.'/'.l:ruby_host
  finally
    if !filereadable(g:ruby_host_prog)
      throw "Z:NotFound neovim-ruby-host".g:ruby_host_prog
      unlet g:ruby_host_prog
    endif
  endtry
endfunc


func! z#constants#globals#Vimpager() abort
    " V1
    let g:vimpager = {
        \ 'enabled'     : 1,
        \ 'X11'         : 0,
        \ 'ansiesc'     : 1,
        \ 'passthrough' : 1,
    \ }
    let g:less     = {
        \ 'enabled'     : 1,
        \ 'number'      : 1,
        \ 'hlsearch'    : 1,
        \ 'scrolloff'   : 5,
    \ }
    " V2
    let g:vimpager_use_gvim        = 0
    let g:vimpager_passthrough     = 1
    let g:vimpager_disable_x11     = 1
    let g:vimpager_scrolloff       = 5
    let g:vimpager_disable_ansiesc = 0
endfunc


func! z#constants#globals#Rustc_path() abort
    if !executable('rustup') || !executable('rustc')
      throw 'missing binaries from $PATH: rustup or rustc'
    endif
    return system("rustup which rustc | xargs printf '%s'")
endfunc
