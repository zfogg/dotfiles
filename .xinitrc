#!/bin/sh

export BROWSER="chromium"
export DE="gnome"


# Map capslock to left-control.
setxkbmap -option ctrl:nocaps


case $1 in
openbox|*)
  exec openbox-session
  ;;
esac

