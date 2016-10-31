#!/usr/local/bin/zsh
# vim: set fdm=marker:
#   ~/.zshenv



if [[ -z "$DOTFILES_SETENV" ]]; then
    . ~/bin/gg.zfo.Dotfiles.setenv.sh
    pkill -flai $TERM_PROGRAM
fi

# zsh startup debug (TOP of ~/.zshenv) {{{
#   https://kev.inburke.com/kevin/profiling-zsh-startup-time
if [[ ! -z "$SHELL_DEBUG" ]]; then
    setopt promptsubst
    zmodload zsh/datetime
    PS4=$'\012# %x    %i\012# %N    +$EPOCHREALTIME\012\011'
    zsh_debug_log="$TMPDIR"/"$ZSH_NAME"-"$ZSH_VERSION"."$$".log
    export SHELL_DEBUG_LOG=`grealpath "$zsh_debug_log"`
    alias shell-debug-log='$EDITOR '"$SHELL_DEBUG_LOG"' ;'
    echo '# vim: ft=zsh nowrap nofen fdm=manual:' > "$SHELL_DEBUG_LOG"
    setopt xtrace
    exec 3>&2 2>>$SHELL_DEBUG_LOG
fi
# zsh startup debug (TOP of ~/.zshenv) }}}
