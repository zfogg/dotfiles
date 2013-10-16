#!/bin/sh

export BROWSER=chromium
export DE=openbox


case $1 in
openbox|*)
  exec openbox-session
  ;;
esac

