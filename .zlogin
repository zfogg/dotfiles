#!/usr/bin/env zsh
# vim: set fdm=marker:


# zsh startup debug (BOTTOM of ~/.zlogin) {{{
#   https://kev.inburke.com/kevin/profiling-zsh-startup-time
if [[ ! -z "$SHELL_DEBUG" ]]; then
    unsetopt xtrace
    exec 2>&3 3>&-
fi
# zsh startup debug (BOTTOM of ~/.zlogin) }}}
