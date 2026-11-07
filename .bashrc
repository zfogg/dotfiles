#!/usr/bin/env bash

#source "${HOME}/.profile"

[ -f "$SYSCONFDIR/profile.d/bash_completion.sh" ] && source "$SYSCONFDIR/profile.d/bash_completion.sh"

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

export AWS_PROFILE=softmax

[ -f "$HOME/.local/share/bin/env" ] && . "$HOME/.local/share/bin/env"

# Vite+ bin (https://viteplus.dev)
if [ -f "$HOME/.vite-plus/env" ]; then
  . "$HOME/.vite-plus/env"
elif [ -f "$HOME/.config/vite-plus/env" ]; then
  . "$HOME/.config/vite-plus/env"
fi
