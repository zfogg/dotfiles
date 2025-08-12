#!/usr/bin/env bash


#source "${HOME}/.profile"

source "$SYSCONFDIR/profile.d/bash_completion.sh"

source "${HOME}/.asdf/plugins/java/set-java-home.bash"

source "${HOME}/.inputrc"

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

export AWS_PROFILE=softmax
