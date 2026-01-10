#!/usr/bin/env zsh
# vim: set fdm=marker:

# Update terminal size when window is resized (SIGWINCH)
TRAPWINCH() {
  # Refresh the ZLE display if we're in the line editor  
  [[ -o zle ]] && zle && zle reset-prompt
}
