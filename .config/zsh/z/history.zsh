#!/usr/bin/env zsh
# vim: set fdm=marker:


[[ -z "$HISTFILE" ]] && \
  export HISTFILE="${ZDOTDIR:-~/.config/zsh}/history"

# NOTE: you should set $SAVEHIST to be no more than $HISTSIZE
export HISTSIZE=65536       # == 256**2
export SAVEHIST=$HISTSIZE

set +o histexpand

setopt   ALWAYS_TO_END
setopt   APPEND_HISTORY
setopt   AUTO_LIST
setopt   AUTO_MENU
setopt   AUTO_PARAM_SLASH
setopt   AUTO_PUSHD
setopt   BANG_HIST
unsetopt BEEP
unsetopt CASE_GLOB
setopt   CDABLEVARS
unsetopt COMPLETE_ALIASES
setopt   COMPLETE_IN_WORD
setopt   EXTENDED_GLOB
setopt   EXTENDED_HISTORY
unsetopt FLOW_CONTROL
setopt   GLOB_COMPLETE
setopt   GLOB_DOTS
setopt   HIST_EXPIRE_DUPS_FIRST
setopt   HIST_IGNORE_ALL_DUPS
setopt   HIST_IGNORE_DUPS
setopt   HIST_IGNORE_SPACE
setopt   HIST_REDUCE_BLANKS
unsetopt HIST_SAVE_NO_DUPS
setopt   HIST_FIND_NO_DUPS
setopt   HIST_VERIFY
setopt   IGNORE_EOF
setopt   INC_APPEND_HISTORY
setopt   INTERACTIVE_COMMENTS
unsetopt LIST_AMBIGUOUS
unsetopt MENU_COMPLETE
setopt   MULTIOS
unsetopt NOMATCH
unsetopt NOTIFY
setopt   NUMERIC_GLOB_SORT
setopt   PATH_DIRS
setopt   PROMPT_SUBST
setopt   PUSHDMINUS
setopt   PUSHD_IGNORE_DUPS
setopt   PUSHD_TO_HOME
setopt   RC_EXPAND_PARAM
setopt   RM_STAR_WAIT
setopt   SHARE_HISTORY
setopt   TRANSIENT_RPROMPT
