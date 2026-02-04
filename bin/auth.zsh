#!/usr/bin/env zsh

set -e

echo yo | gpg --clearsign

ssh-add
[ -f ~/.ssh/aur ] && ssh-add ~/.ssh/aur
