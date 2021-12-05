#!/usr/bin/env zsh
# vim:ft=zsh


(
  autoload -U zrecompile

  setopt EXTENDEDGLOB

  local zfiles=(
    "$ZDOTDIR/.zcompdump"
    "$ZDOTDIR"/.{zshrc,zshenv,zprofile,zlogin}
    "$ZDOTDIR"/z/*.zsh
  )

  for zf in $zfiles; do
    #/bin/rm -f "${zf}.zwc"
    if [[ -s "$zf" && (! -s "${zf}.zwc" || "$zf" -nt "${zf}.zwc") ]]; then
      #echo "zrecompile ${zf}.zwc"
      zrecompile -pq "$zf"
    fi
  done

  for a in $fpath; do
    #/bin/rm -f "${a}.zwc"
    if [[ -d "$a" && -w "$a" ]]; then
      #echo "zrecompile ${a}.zwc"
      #zrecompile -p "${a}.zwc" "$a"/_*~"$a"/__*
    fi
  done

) &!
