#!/usr/bin/env zsh
#
# https://github.com/zsh-users/zsh-autosuggestions


export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=8'


# Prefix to use when saving original versions of bound widgets
#ZSH_AUTOSUGGEST_ORIGINAL_WIDGET_PREFIX=__au-og__
#ZSH_AUTOSUGGEST_STRATEGY=match_prev_cmd
export ZSH_AUTOSUGGEST_STRATEGY=default

#Widgets that clear the suggestion
export ZSH_AUTOSUGGEST_CLEAR_WIDGETS=(
    history-search-forward
    history-search-backward
    history-beginning-search-forward
    history-beginning-search-backward
    history-substring-search-up
    history-substring-search-down
    up-line-or-history
    down-line-or-history
    accept-line
)

# Widgets that accept the entire suggestion
export ZSH_AUTOSUGGEST_ACCEPT_WIDGETS=(
    vi-end-of-line
    vi-add-eol
)

# Widgets that accept the entire suggestion and execute it
export ZSH_AUTOSUGGEST_EXECUTE_WIDGETS=(
    accept-line
)

# Widgets that accept the suggestion as far as the cursor moves
export ZSH_AUTOSUGGEST_PARTIAL_ACCEPT_WIDGETS=(
    forward-word
    vi-forward-word
    vi-forward-word-end
    vi-forward-blank-word
    vi-forward-blank-word-end
    forward-char
    vi-forward-char
)
