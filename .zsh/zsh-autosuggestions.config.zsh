#!/usr/bin/env zsh
#
# INFO: https://github.com/zsh-users/zsh-autosuggestions


# Set ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE to configure the style that the 
# suggestion is shown with. The default is fg=8, which will set the foreground 
# color to color 8 from the 256-color palette. If your terminal only supports 8 
# colors, you will need to use a number between 0 and 7.
#
# Background color can also be set, and the suggestion can be styled bold, 
# underlined, or standout. For example, this would show suggestions with bold, 
# underlined, pink text on a cyan background:
#export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#ff00ff,bg=cyan,bold,underline"
#export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=07,bg=18,underline'
#export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=07,bg=underline'
#export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=07,bg=bold,underline'
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=08,bg=bold,underline'

# Set ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE to an integer value to disable 
# autosuggestion for large buffers. The default is unset, which means that 
# autosuggestion will be tried for any buffer size. Recommended value is 20. 
# This can be useful when pasting large amount of text in the terminal, to 
# avoid triggering autosuggestion for strings that are too long.
export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE='20'

# As of v0.4.0, suggestions can be fetched asynchronously. To enable this 
# behavior, set the ZSH_AUTOSUGGEST_USE_ASYNC variable (it can be set to 
# anything).
export ZSH_AUTOSUGGEST_USE_ASYNC='1'

# Prefix to use when saving original versions of bound widgets
#export ZSH_AUTOSUGGEST_ORIGINAL_WIDGET_PREFIX='autosuggest-orig'-


# ZSH_AUTOSUGGEST_STRATEGY is an array that specifies how suggestions should 
#   be generated. The strategies in the array are tried successively until a 
#   suggestion is found. There are currently three built-in strategies to choose 
#
# from:
#   - `history`: Chooses the most recent match from history.
#   - `completion`: Chooses a suggestion based on what tab-completion would 
#       suggest. (requires `zpty` module)
#   - `match_prev_cmd`: Like `history`, but chooses the most recent match whose 
#       preceding history item matches the most recently executed command
#       ([more info](src/strategies/match_prev_cmd.zsh)). Note that this strategy
#       won't work as expected with ZSH options that don't preserve the history order
#       such as `HIST_IGNORE_ALL_DUPS` or `HIST_EXPIRE_DUPS_FIRST`.
#
# For example, setting `ZSH_AUTOSUGGEST_STRATEGY=(history completion)` will 
#   first try to find a suggestion from your history, but, if it can't find a 
#   match, will find a suggestion from the completion engine.
#
#export ZSH_AUTOSUGGEST_STRATEGY=match_prev_cmd
#export ZSH_AUTOSUGGEST_STRATEGY=(history completion)
export ZSH_AUTOSUGGEST_STRATEGY=(match_prev_cmd completion)

#Widgets that clear the suggestion
export ZSH_AUTOSUGGEST_CLEAR_WIDGETS=(
    history-search-forward
    history-search-backward
    history-beginning-search-forward
    history-beginning-search-backward
    history-substring-search-up
    history-substring-search-down
    up-line-or-beginning-search
    down-line-or-beginning-search
    up-line-or-history
    down-line-or-history
    accept-line
    copy-earlier-word
)

# Widgets that accept the entire suggestion
export ZSH_AUTOSUGGEST_ACCEPT_WIDGETS=(
    forward-char
    end-of-line
    vi-forward-char
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
    #emacs-forward-word
    vi-forward-word
    vi-forward-word-end
    vi-forward-blank-word
    vi-forward-blank-word-end
    vi-find-next-char
    vi-find-next-char-skip
)

# Widgets that should be ignored (globbing supported but must be escaped)
export ZSH_AUTOSUGGEST_IGNORE_WIDGETS=(
    orig-\*
    beep
    run-help
    set-local-history
    which-command
    yank
    yank-pop
    zle-\*
)

