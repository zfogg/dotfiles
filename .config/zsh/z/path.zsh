#!/usr/bin/env zsh
# vim: set fdm=marker:
#   ~/.config/zsh/z/path.zsh

# {{{ variable typesets
  # NOTE: these typesets need to appear first
  typeset -U   path
  typeset -U   fpath
  typeset -U   manpath
  typeset -aU  infopath
  typeset -xUT INFOPATH infopath

  # INFO: https://www.reddit.com/r/neovim/comments/ga0s7w/comment/foxs2m8/
  [[ "$SHLVL" > 1 ]] && return
# }}}

#print -rl -- "${fpath[@]#/usr/local/share/zsh/site-functions}"
#sleep 10

function() { # {{{ BEFORE platform-specifics
  path=(
    "$path[@]"
  )

  fpath=(
    #/usr/share/zsh/site-functions
    "$fpath[@]"
  )

  # MANPATH initialized in platform-specifics sections below

  infopath=(
    "$infopath[@]"
  )
} # }}}

function () { # {{{ platform-specifics
  if [[ ${OSX:-0} == ${TRUE:-1} ]]; then
    path=(
      $BREW/opt/ccache/libexec
      $BREW/opt/{findutils,coreutils,libtool}/libexec/gnubin
      $BREW/opt/{make,grep,ed,gawk}/libexec/gnubin
      $BREW/opt/gnu-{sed,tar,which,indent}/libexec/gnubin
      $BREW/opt/{curl,gnu-getopt,file-formula,unzip}/bin
      $BREW/opt/llvm/bin
      $BREW/{bin,sbin}
      /usr/local/bin
      /Library/Developer/CommandLineTools/usr/bin
      "$path[@]"
      $(getconf PATH | command -p tr ':' '\n'))

    fpath=(
      $BREW/share/zsh/site-functions
      $HOME/.docker/completions
      "$fpath[@]")

    manpath=(
      $BREW/opt/{findutils,coreutils,libtool}/libexec/gnuman
      $BREW/opt/{make,grep,ed,gawk}/libexec/gnuman
      $BREW/opt/gnu-{sed,tar,which,indent}/libexec/gnuman
      $BREW/opt/{curl,gnu-getopt,file-formula,unzip}/share/man
      $BREW/share/man
      /usr/local/share/man
      /usr/share/man
      /opt/local/share/man)

    infopath=(
      $BREW/share/info
      /usr/share/info
      #"$infopath[@]"
    )

  elif [[ ${LINUX:-0} == ${TRUE:-1} ]]; then

    if command -v ccache 2> /dev/null >&2 && [[ -f $BREW/bin/ccache ]]; then
      export CCACHE_PATH=$BREW/bin
    fi

    local latest_ruby=""
    local ruby_dirs=($HOME/.local/share/gem/ruby/*/bin(NnF/[-1]))
    (( ${#ruby_dirs} )) && latest_ruby=$ruby_dirs[1]
    path=(
      $HOME/.local/share/../bin # uv, uvx
      $latest_ruby
      #$HOME/.ghcup/bin
      $BREW/lib/colorgcc/bin
      $BREW/lib/ccache/bin
      $BREW/local/{bin,sbin}
      $BREW/{bin,sbin}
      /usr/lib/emscripten
      "$path[@]"
      /bin
      /sbin
    )

    fpath=(
      $BREW/share/zsh/{site-functions,functions}
      ${(@f)fpath#/usr/local/share/zsh/site-functions}
      #"$fpath[@]"
    )

    manpath=(
      $BREW/share/man
      /usr/local/share/man
      /usr/share/man
      "$manpath[@]"
    )

    infopath=(
      $BREW/share/info
      $XDG_DATA_HOME/info
      #"$infopath[@]"
    )
  fi
} # }}}

function() { # {{{ AFTER platform-specifics
  #local nvmv_root=($HOME/.nvm/versions/node/v*.*.*([-1]))
  #echo $HOME/.nvm/versions/node/v*.*.*/bin(nF/[-1])

  path=(
    $HOME/bin
    $HOME/.local/bin
    $HOME/.{cabal,cargo}/bin
    $HOME/.local/share/asdf/shims
    #/nix/var/nix/profiles/default/bin
    #$HOME/.nvm/versions/node/v*.*.*/bin(nF/[-1])
    #$HOME/.rbenv/shims
    #$HOME/.gem/ruby/*.*.*/bin(nF/[-1])
    # NOTE: PYENV_ROOT+PATH are set by pyenv-lazy via antigen
    #$HOME/.pyenv/{bin,shims}
    $GOPATH/bin
    "$path[@]"
  )

  if [[ -d ~/.fzf/bin ]]; then
    path=(
      $HOME/.fzf/bin
      "$path[@]")
  fi

  if (( ${+VIRTUAL_ENV} )); then
    path=(
      $VIRTUAL_ENV/bin
      "$path[@]")
  fi

  #if [[ -d ~/.asdf/completions ]]; then
    #fpath=(
      #$HOME/.asdf/completions
      #"$fpath[@]")
  #fi

  #if [[ -v ANDROID_HOME ]]; then
    #path=(
      #$ANDROID_HOME/emulator
      #$ANDROID_HOME/tools/bin
      #$ANDROID_HOME/platform-tools
      #$ANDROID_HOME/build-tools/${ANDROID_SDK_VERSION:-*.*.*(nF/[-1])}
      #"$path[@]"
    #)
  #fi

  fpath=(
    $HOME/.zsh/site-functions
    "$fpath[@]"
  )

  manpath=(
    $XDG_DATA_HOME/man
    "$manpath[@]"
  )

  #infopath=(
    #$XDG_DATA_HOME/info
    #$BREW/share/info
    #"$infopath[@]"
  #)
} # }}}


# {{{ variables exports
  # FIXME: do i even need these exports?
    export PATH
    export FPATH
    export MANPATH
    export CLASSPATH
    export INFOPATH
# }}}


# {{{ reorder MANPATH - antidote first, then system paths
export MANPATH="$(
  {
    [[ -d $HOME/.local/share/antidote/man ]] && echo "$HOME/.local/share/antidote/man"
    [[ -d $XDG_DATA_HOME/man ]] && echo "$XDG_DATA_HOME/man"
    [[ -d $BREW/share/man ]] && echo "$BREW/share/man"
    echo "/usr/local/share/man"
    echo "/usr/share/man"
  } | paste -sd: -
)"
# }}}
