#!/usr/bin/env sh


function current_shell() {
    current_shell=`ps -p $$ | awk '$1 != "PID" {print $(NF)}' | grep -Eo '(sh|bash|zsh|fish)'`
    basename "$current_shell"
}

function command_exists() {
    command -v "$1" 2>/dev/null 1>&2
}

export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_DATA_HOME="${HOME}/.local/share"

export  TRUE='1'
export FALSE='0'

export OSX=$(
  [[ $OSTYPE =~ darwin ]]
  [[ $? == 0 ]] \
    && echo "${TRUE:-1}" \
    || echo "${FALSE-0}")

export LINUX=$(
  [[ $OSTYPE =~ linux ]]
  [[ $? == 0 ]] \
    && echo "${TRUE:-1}" \
    || echo "${FALSE-0}")

if [[ $OSX == $TRUE ]]; then
    export BREW='/usr/local'
    export HOMEBREW_CLEANUP_MAX_AGE_DAYS='2'
elif [[ $LINUX == $TRUE ]]; then
    export BREW='/usr'
else
    export BREW='/usr/local'
fi

export SYSCONFDIR="${BREW}/etc"

export SHELL_NAME=`current_shell`
