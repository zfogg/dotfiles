" autoload/z/constants/globals
scriptencoding utf-8


func! z#constants#globals#Ft()
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

func! z#constants#globals#ConfigPaths()
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


func! z#constants#globals#Python()
  if has('win32')
    let l:py3_prog = 'python3.exe'
    let l:py3_root = $USERPROFILE.'/scoop/shims'
  elseif has('unix')
    let l:py3_prog = 'python3'
    if exists('$PYENV_ROOT')
      let l:py3_root = $PYENV_ROOT.'/versions/neovim/bin'
    elseif exists('$BREW')
      let l:py3_root = $BREW.'/bin'
    endif
  endif
  try
    let g:python3_host_prog = l:py3_root.'/'.l:py3_prog
  catch /^Vim(\a\+):E121:/
    echoerr 'Z:NotFound l:py3_root | l:py3_prog'
    if !filereadable(g:python3_host_prog)
      echoerr 'Z:NotFound python3-host-prog '.g:python3_host_prog
      unlet g:python3_host_prog
    endif
  finally
    if has('win32')
      let g:python3_host_prog = fnamemodify(g:python3_host_prog, ':r')
    endif
  endtry
endfunc


func! z#constants#globals#Nodejs()
  try
    if has('unix')
      let l:node_host = 'neovim-node-host'
      let l:which_prog = systemlist('which '.l:node_host)[0]
      if v:shell_error != 0 | echom 'Z:NotFound 1 '.l:node_host | endif
      let l:host_path = fnamemodify(l:which_prog, ':p:h')
    elseif has('win32')
      " FIXME
      return
      let g:loaded_node_provider = 0
      let l:node_host = 'neovim-node-host.cmd'
      let l:host_path = expand($USERPROFILE).'/scoop/persist/nodejs/bin'
    endif
    let g:node_host_prog = l:host_path.'/'.l:node_host
  finally
    if exists('g:node_host_prog') && !filereadable(g:node_host_prog)
      echom 'Z:NotFound 2 neovim-node-host '.g:node_host_prog
      unlet g:node_host_prog
    endif
  endtry
endfunc


func! z#constants#globals#Ruby()
  try
    if has('unix')
      let l:ruby_host = 'neovim-ruby-host'
      let l:which_prog = systemlist('which '.l:ruby_host)[0]
      if v:shell_error != 0 | echom 'Z:NotFound '.l:ruby_host | endif
      let l:host_path = fnamemodify(l:which_prog, ':p:h')
    elseif has('win32')
      let l:ruby_host = 'neovim-ruby-host.bat'
      let l:host_path = expand($USERPROFILE).'/scoop/apps/ruby/current/gems/bin'
    endif
    let g:ruby_host_prog = l:host_path.'/'.l:ruby_host
  finally
    if exists('g:ruby_host_prog') && !filereadable(g:ruby_host_prog)
      echom 'Z:NotFound neovim-ruby-host'.g:ruby_host_prog
      unlet g:ruby_host_prog
    endif
  endtry
endfunc


func! z#constants#globals#Rustc_path()
  if executable('rustup')
    let s:rustc_path = system('rustup which rustc | xargs printf')
  else
    let s:rustc_path = exepath('rustc')
  endif
  if !executable(s:rustc_path)
    echom 'Z:NotFound rustup|rustc - '.s:rustc_path
  endif
  return s:rustc_path
endfunc

