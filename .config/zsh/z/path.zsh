#!/usr/bin/env zsh
# vim: set fdm=marker:


# {{{ Typeset: path | fpath | manpath | classpath | infopath
  # NOTE: these typesets need to appear first
  typeset -U path
  typeset -U fpath
  typeset -U manpath

  typeset -aU classpath
  typeset -xT CLASSPATH classpath
  typeset -aU infopath
  typeset -xT INFOPATH infopath
# }}}


function() { # BEFORE platform-specific paths
  path=(
    "$path[@]")

  fpath=(
    /usr/share/zsh/site-functions
    "$fpath[@]")

  manpath=(
    /usr/share/man
    "$manpath[@]")

  infopath=(
    /usr/share/info
    "$infopath[@]")
}


function () { # platform-specific paths
  if [[ "${OSX:-0}" == "${TRUE:-1}" ]]; then
    path=(
      $BREW/opt/fzf/bin
      $BREW/{sbin,bin}
      "$path[@]"
      $(getconf PATH | command -p tr ':' '\n' | command -p tail -r))

    fpath=(
      $BREW/share/zsh/{site-functions,functions}
      "$fpath[@]")

    manpath=(
      $BREW/share/man
      "$manpath[@]")

    infopath=(
      $BREW/share/info
      "$infopath[@]")

  elif [[ "${LINUX:-0}" == "${TRUE:-1}" ]]; then
    #path=(
      #"$path[@]")

    #fpath=(
      #"$fpath[@]")

    #manpath=(
      #"$manpath[@]")

    #infopath=(
      #"$infopath[@]")
  fi
}


function() { # AFTER platform-specific paths
  path=(
    $HOME/bin
    $HOME/.local/bin
    $HOME/.{cabal,cargo,gem}/bin
    # NOTE: PYENV_ROOT+PATH are set by pyenv-lazy via antigen
    #$PYENV_ROOT/{bin,shims}
    {$GOPATH,$GOROOT}/bin
    "$path[@]")

  fpath=(
    $HOME/.zsh/{site-functions,complete}
    "$fpath[@]")

  manpath=(
    $XDG_DATA_HOME/man
    "$manpath[@]")

  infopath=(
    $XDG_DATA_HOME/info
    "$infopath[@]")
}


# {{{ Export: PATH | FPATH | MANPATH | CLASSPATH | INFOPATH
  # FIXME: do i even need these exports?
  #export PATH
  #export FPATH
  #export MANPATH
  #export CLASSPATH
  #export INFOPATH
# }}}

