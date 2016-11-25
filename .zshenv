#!/usr/local/bin/zsh
# vim: set fdm=marker:



if [[ -z "$DOTFILES_SETENV" ]]; then
    . ~/bin/gg.zfo.Dotfiles.setenv.sh
    pkill -flai $TERM_PROGRAM
fi

# zsh startup debug (TOP of ~/.zshenv) {{{
#   https://kev.inburke.com/kevin/profiling-zsh-startup-time
if [[ ! -z "$SHELL_DEBUG" ]]; then
    zsh_debug_log="$TMPDIR"'/debug.'"$ZSH_NAME"-"$ZSH_VERSION"'.'"$$"'.zsh'
    export SHELL_DEBUG_LOG=`grealpath "$zsh_debug_log"`
    echo "# vim: fdm=marker fen:" > "$SHELL_DEBUG_LOG"
    printf "#{{{" >> "$SHELL_DEBUG_LOG"
    function shell-debug-log {
        . ~/.zlogin
        echo "#}}}" >> "$SHELL_DEBUG_LOG"
        $EDITOR "$SHELL_DEBUG_LOG"
        exit
    }
    setopt promptsubst && zmodload zsh/datetime
    PS4=$'#}}}\012\012\012# %x {{{\012# %N:%i\012# +$EPOCHREALTIME\012  '
    setopt xtrace
    exec 3>&2 2>>$SHELL_DEBUG_LOG
fi
# zsh startup debug (TOP of ~/.zshenv) }}}
