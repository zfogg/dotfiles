#!/usr/bin/env zsh
# vim: set fdm=marker:


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
    ~/.local/bin
    ~/.{cargo,gem}/bin
    #~/.cabal/bin
    #~/.platformio/penv/bin
    $PYENV_ROOT/{bin,shims}
    {$GOPATH,$GOROOT}/bin
    $BREW/MacGPG2/bin
    $BREW/opt/fzf/bin
    $BREW/{sbin,bin}
    "$path[@]"
    #$(/usr/bin/getconf PATH | /usr/bin/tr ':' '\n' | /usr/bin/tail -r)
  )

else
  path=(
    ~/bin
    ~/.local/bin
    ~/.{cabal,cargo,gem}/bin
    $PYENV_ROOT/shims
    $GOPATH/bin
    $GOROOT/bin
    "$path[@]")
fi

fpath_rm=('/usr/local/share/zsh/site-functions')

fpath=(
  $HOME/.zsh/{site-functions,complete}
  $BREW/share/zsh/{site-functions,functions}
  ${fpath[@]/$fpath_rm})

manpath=(
  /usr/share/man
  "$manpath[@]")

#if [[ "$OSX" == "$TRUE" ]]; then
  #path=(
    #$BREW/opt/coreutils/libexec/gnubin
    #$BREW/opt/findutils/libexec/gnubin
    #$BREW/opt/gnu-tar/libexec/gnubin
    #$BREW/opt/ccache/libexec
    #$BREW/MacGPG2/bin
    #"$path[@]"
  #)

  ##fpath=(
  ##  $fpath
  ##)

  #manpath=(
    ##${BREW}/Cellar/*/*/{share/man,libexec/gnuman}
    #${BREW}/share/man
    #$manpath
  #)

  #infopath=(
    #$infopath
  #)
#fi

#export PATH
#export MANPATH
#export FPATH
#export CLASSPATH
#export INFOPATH
