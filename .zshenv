#!/usr/bin/env zsh
# vim: set fdm=marker:

#if [[ -z "$DOTFILES_SETENV" && -o login ]]; then
if [[ ! -v DOTFILES_SETENV && -o interactive ]]; then
  if [[ -v TERM_PROGRAM ]]; then
    echo 'eval "`~/bin/local.launchd`"'
    eval "`~/bin/local.launchd`"
    echo "$DOTFILES_SETENV"
    pkill -fla "$TERM_PROGRAM"
  else
    echo "> source ~/.launchd.conf"
    source ~/.launchd.conf
  fi
fi


typeset -U fpath
fpath=(
  ~/.zsh/complete
  "$BREW"/share/zsh-completions
  ~/.zsh/site-functions
  "${BREW}/share/zsh/"{site-functions,functions}
  "${fpath[@]}"
)


#if [[ -n $VIRTUAL_ENV && -e "${VIRTUAL_ENV}/bin/activate" ]]; then;
  #source "${VIRTUAL_ENV}/bin/activate"; fi


# zsh startup debug (TOP of ~/.zshenv) {{{
#   https://kev.inburke.com/kevin/profiling-zsh-startup-time
if [[ ! -z "$SHELL_DEBUG" ]]; then
  local zsh_debug_log=`~/bin/mktempf "${ZSH_NAME}-${ZSH_VERSION}.${$}.zsh"`
  SHELL_DEBUG_LOG=`grealpath "$zsh_debug_log"`
  echo "# vim: fdm=marker fen:" > "$SHELL_DEBUG_LOG"
  printf "#{{{" >> "$SHELL_DEBUG_LOG"
  function shell-debug-log {
    . ~/.zlogin
    echo "#}}}" >> "$SHELL_DEBUG_LOG"
    echo "$SHELL_DEBUG_LOG"
    sed -i 's|^#{{{#}}}$||'  "$SHELL_DEBUG_LOG"
    sed -i 's|'"$HOME"'|~/|' "$SHELL_DEBUG_LOG"
    $EDITOR "$SHELL_DEBUG_LOG"
    exit
  }
  zmodload zsh/datetime; setopt promptsubst
  PS4=$'#}}}\012\012\012# %x:%I {{{\012# %N:i \012# +$EPOCHREALTIME\012  '
  setopt xtrace
  exec 3>&2 2>$SHELL_DEBUG_LOG
fi
# zsh startup debug (TOP of ~/.zshenv) }}}
