#!/usr/bin/env bash

#source "${HOME}/.profile"

[ -f "$SYSCONFDIR/profile.d/bash_completion.sh" ] && source "$SYSCONFDIR/profile.d/bash_completion.sh"

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

export AWS_PROFILE=softmax

[ -f "$HOME/.local/share/bin/env" ] && . "$HOME/.local/share/bin/env"
