#!/usr/bin/env zsh
# vim: set fdm=marker:


# {{{ variable typesets
  # NOTE: these typesets need to appear first
  typeset -U path
  typeset -U fpath
  typeset -U manpath

  typeset -aU classpath
  typeset -xT CLASSPATH classpath
  typeset -aU infopath
  typeset -xT INFOPATH infopath
# }}}


function() { # {{{ BEFORE platform-specifics
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
} # }}}


function () { # {{{ platform-specifics
  if [[ ${OSX:-0} == ${TRUE:-1} ]]; then
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

  elif [[ ${LINUX:-0} == ${TRUE:-1} ]]; then
    path=(
      "$path[@]")

    fpath=(
      $BREW/share/zsh/{site-functions,functions}
      "$fpath[@]")

    manpath=(
    $BREW/share/man
      "$manpath[@]")

    infopath=(
      $BREW/share/info
      "$infopath[@]")
  fi
} # }}}


function() { # {{{ AFTER platform-specifics
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
    $HOME/.nix-profile/share/man
    $XDG_DATA_HOME/man
    "$manpath[@]")

  infopath=(
    $HOME/.nix-profile/share/man
    $XDG_DATA_HOME/info
    "$infopath[@]")
} # }}}


# {{{ variables exports
  # FIXME: do i even need these exports?
  #export PATH
  #export FPATH
  #export MANPATH
  #export CLASSPATH
  #export INFOPATH
# }}}

