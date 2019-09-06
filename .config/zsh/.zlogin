#!/usr/bin/env zsh
# vim:syntax=zsh
# vim:filetype=zsh

(
  autoload -U zrecompile

  setopt EXTENDEDGLOB

  local zfiles=(
    "${ZDOTDIR}/.zcompdump"
    "${ZDOTDIR}/.zshrc"
    "${ZDOTDIR}/.zprofile"
    "${ZDOTDIR}/.zshenv"
  )

  for zf in $zfiles; do
    #/bin/rm -f "${zf}.zwc"
    if [[ -s "$zf" && (! -s "${zf}.zwc" || "$zf" -nt "${zf}.zwc") ]]; then
      echo "compile ${zf}.zwc yo"
      zrecompile -pq "$zf"
    fi
  done

  for a in $fpath; do
    #/bin/rm -f "${a}.zwc"
    if [ -d $a ]; then
      zrecompile -p "${a}.zwc" "$a"/_*~"$a"/__*
    fi
  done

) &!

