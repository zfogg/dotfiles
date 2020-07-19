#!/usr/bin/env zsh
# vim: set fdm=marker:


# {{{ variable typesets
  # NOTE: these typesets need to appear first
  typeset -U   path
  typeset -U   fpath
  typeset -U   manpath
  typeset -aU  infopath
  typeset -xUT INFOPATH infopath

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
    "$infopath[@]")
} # }}}

function () { # {{{ platform-specifics
  if [[ ${OSX:-0} == ${TRUE:-1} ]]; then
    path=(
      $HOME/.rbenv/shims
      $BREW/opt/ruby/bin
      $BREW/{bin,sbin}
      #/Library/Developer/CommandLineTools/usr/bin
      "$path[@]"
      $(getconf PATH | command -p tr ':' '\n'))

    fpath=(
      $BREW/share/zsh/{site-functions,functions}
      "$fpath[@]")

    manpath=(
      $BREW/share/man
      "$manpath[@]")

    infopath=(
      $BREW/share/info
      /usr/share/info
      #"$infopath[@]"
    )

  elif [[ ${LINUX:-0} == ${TRUE:-1} ]]; then
    path=(
      $BREW/local/{bin,sbin}
      $BREW/{bin,sbin}
      #/usr/share/aws-cli/v2/2.0.14/bin
      "$path[@]"
      /bin
      /sbin
    )

    fpath=(
      $BREW/share/zsh/{site-functions,functions}
      "$fpath[@]")

    manpath=(
      $BREW/share/man
      "$manpath[@]")

    infopath=(
      $BREW/share/info
      $XDG_DATA_HOME/info
      #"$infopath[@]"
    )
  fi
} # }}}

function() { # {{{ AFTER platform-specifics
  local nvmv=$(<~/.nvm/alias/$(<~/.nvmrc))
  local nvmv_root=versions/node/v$nvmv

  path=(
    $HOME/bin
    $HOME/.local/bin
    $HOME/.nvm/$nvmv_root/bin
    $HOME/.{cabal,cargo,gem}/bin
    $HOME/.gem/ruby/2.7.0/bin
    # NOTE: PYENV_ROOT+PATH are set by pyenv-lazy via antigen
    #$PYENV_ROOT/{bin,shims}
    $GOPATH/bin
    "$path[@]")

    if [[ -v ANDROID_HOME ]]; then
      path=(
        $ANDROID_HOME/emulator
        $ANDROID_HOME/tools/bin
        $ANDROID_HOME/platform-tools
        $ANDROID_HOME/build-tools/${ANDROID_SDK_VERSION:-*.*.*([-1])}
        "$path[@]")
    fi

  fpath=(
    $HOME/.zsh/{site-functions,complete}
    "$fpath[@]")

  manpath=(
    $HOME/.nvm/$nvmv_root/share/man
    $XDG_DATA_HOME/man
    "$manpath[@]")

  #infopath=(
    #$XDG_DATA_HOME/info
    #$BREW/share/info
    #"$infopath[@]")
} # }}}


# {{{ variables exports
  # FIXME: do i even need these exports?
  #export PATH
  #export FPATH
  #export MANPATH
  #export CLASSPATH
  #export INFOPATH
# }}}

