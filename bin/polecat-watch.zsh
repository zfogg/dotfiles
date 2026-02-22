#!/usr/bin/env zsh

PREFIX="as-"

while true; do
  for polecat in `ls ~/gt/asciichat/polecats`; do

    #set -x
    tmux_session=$(tmux list-sessions | grep "$polecat" | sed 's/:.*//' | grep -E "^$PREFIX")
    echo "Checking if $polecat ($tmux_session) is attached..."

    # Random sleep: if 2+ quit at the same time so they'd run tmux 
    # list-sessions and see an unattached session and attach to the same 
    # session without this
    sleep "$((RANDOM / 10000.0))"
    tmux list-sessions | grep "$polecat" | grep -E '\(attached\)$' >/dev/null 2>&1
    tmux_session_is_attached=$?

    if [ ! "$tmux_session_is_attached" -eq "0" ] && ! echo "$tmux_session" | grep -qE "^hq-"; then
      if [ -n "$tmux_session" ]; then
        pane=$(tmux capture-pane -t "$tmux_session" -p)
        echo "$pane" | grep "bypass permissions on"
        can_work=$?

        if [ "$can_work" -eq "0" ] ; then
          echo "Attaching to $polecat - ($tmux_session)"
          tmux a -t "$tmux_session" || >&2 echo "Failed to attach to $polecat ($tmux_session) - can_work=$can_work"
        fi
      fi
    fi
    #set +x
  done

  #exit
  echo "NONE UNATTACHED FOUND"
  sleep 5
  # Loop! Search for more polecats to connect to.
done

