#!/usr/bin/env zsh


while true; do
  for polecat in `ls ~/gt/asciichat/polecats`; do

    tmux_session=$(tmux list-sessions | grep "$polecat" | sed 's/:.*//')
    echo "Checking if $polecat ($tmux_session) is attached..."

    tmux list-sessions | grep "$polecat" | grep -E '\(attached\)$' >/dev/null 2>&1
    tmux_session_is_attached=$?

    if [ ! "$tmux_session_is_attached" -eq "0" ] && ! `echo "$tmux_session" | grep -E "^hq-"`; then
      echo "Attaching to $polecat - ($tmux_session)"
      tmux a -t "$tmux_session" || >&2 echo "Failed to attach to $polecat ($tmux_session)"
    else
      echo "NOPE"
    fi
  done

  sleep 5
  # Loop! Search for more polecats to connect to.
done

