#!/usr/bin/env zsh
# vim: set fdm=marker:


# path, manpath, fpath {{{
typeset -U path
typeset -U fpath
typeset -U manpath

typeset -aU classpath
typeset -xT CLASSPATH classpath
typeset -aU infopath
typeset -xT INFOPATH infopath


if [[ "$OSX" == "$TRUE" ]]; then
  path=(
    ~/bin
    #~/.config/composer/vendor/bin
    #~/.platformio/penv/bin
    #"$BREW"/opt/php71/bin
    "$BREW"/opt/node/bin
    "$BREW"/opt/ruby/bin
    "$PYENV_ROOT"/shims
    #~/.jenv/shims
    ~/.local/bin
    ~/.{cargo,gem}/bin
    #~/.cabal/bin
    "$GOPATH"/bin
    "$GOROOT"/bin
    "$BREW"/sbin
    "$BREW"/MacGPG2/bin
    "$BREW"/bin
    $path
    #$(/usr/bin/getconf PATH | /usr/bin/tr ':' '\n' | /usr/bin/tail -r)
  )

else
  path=(
    ~/bin
    ~/.local/bin
    ~/.{cabal,cargo,gem}/bin
    "$PYENV_ROOT"/shims
    "$GOPATH"/bin
    "$GOROOT"/bin
    "$path[@]")
fi

fpath_rm=('/usr/local/share/zsh/site-functions')

fpath=(
  "${HOME}/.zsh/"{site-functions,complete}
  "${BREW}/share/zsh/"{site-functions,functions}
  ${fpath[@]/$fpath_rm})

manpath=(
  /usr/share/man
  "$manpath[@]")

if [[ "$OSX" == "$TRUE" ]]; then
  local brew_gnu_progs=(
  coreutils
  findutils
  #gnu-sed
  #gnu-which
  gnu-tar)
  path=(
    #"${BREW}/opt/${^brew_gnu_progs}/libexec/gnubin"
    "${BREW}/opt/coreutils/libexec/gnubin"
    "${BREW}/opt/findutils/libexec/gnubin"
    "${BREW}/opt/gnu-tar/libexec/gnubin"
    "${BREW}/opt/ccache/libexec"
    #"$iOSOpenDevPath"/bin
    #"$THEOS"/bin
    "${BREW}/MacGPG2/bin"
    $path
  )
  #fpath=(
  #  $fpath
  #)
  manpath=(
    #${BREW}/Cellar/*/*/{share/man,libexec/gnuman}
    ${BREW}/opt/${^brew_gnu_progs}"/libexec/gnuman"
    ${BREW}/share/man
    $manpath
  )
    infopath=(
      ${BREW}/opt/${^brew_gnu_progs}/share/info
      $infopath
    )
fi

export PATH
export MANPATH
export FPATH
export CLASSPATH
export INFOPATH
# path, manpath, fpath }}}
