#!/usr/bin/env bash


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
if [[ $OSTYPE =~ darwin ]]; then
  echo "${TRUE:-1}"
else
  echo "${FALSE-0}"
fi
)

export LINUX=$(
if [[ "$OSTYPE" =~ linux ]]; then
  echo "${TRUE:-1}"
else
  echo "${FALSE-0}"
fi
)

if [[ $OSX == "$TRUE" ]]; then
  export BREW='/usr/local'
  export HOMEBREW_CLEANUP_MAX_AGE_DAYS='2'
elif [ "$LINUX" = "$TRUE" ]; then
  export BREW='/usr'
else
  export BREW='/usr/local'
fi

if [[ $OSX = "$TRUE" ]]; then
  export SYSCONFDIR="$BREW/etc"
elif [ "$LINUX" = "$TRUE" ]; then
  export SYSCONFDIR="/etc"
fi

export SHELL_NAME=`current_shell`
if [ -e /Users/zfogg/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/zfogg/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
