#!/usr/bin/env zsh
# vim: set fdm=marker:

# Terminal size detection and updates
# Ensure COLUMNS and LINES are set and updated on terminal resize

# Function to update terminal size
update_terminal_size() {
  if [[ -t 0 ]]; then
    # Get terminal size using stty
    local size=($(stty size 2>/dev/null))
    if [[ ${#size} -eq 2 ]]; then
      export LINES=${size[1]}
      export COLUMNS=${size[2]}
    fi
  fi
}

# Update size on shell start
update_terminal_size

# Update size when terminal is resized (SIGWINCH)
TRAPWINCH() {
  update_terminal_size
  # Refresh the ZLE display if we're in the line editor
  [[ -o zle ]] && zle && zle reset-prompt
}
