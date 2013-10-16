#!/bin/sh

export BROWSER=chromium
export DE=openbox


case $1 in
openbox|*)
  exec ck-launch-session dbus-launch openbox-session
  ;;
esac

