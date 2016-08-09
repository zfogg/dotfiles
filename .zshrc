#!/usr/bin/env zsh
# vim: set fdm=marker:

# Prompt. {{{
TRAPWINCH() {
  zle && { zle reset-prompt; zle -R }
}
# }}}


# Modules. {{{
autoload -Uz colors \
    zsh-mime-setup \
    edit-command-line-magic \
    edit-command-line \
    url-quote-magic \
    select-word-style
colors
zsh-mime-setup
zle -N self-insert url-quote-magic
zle -N edit-command-line 
select-word-style bash
# }}}


# Autocomplete. {{{
source $HOME/.zsh/zsh-completion-generator/zsh-completion-generator.plugin.zsh

unsetopt menu_complete
unsetopt LIST_AMBIGUOUS
setopt  COMPLETE_IN_WORD

_force_rehash() {
  (( CURRENT == 1 )) && rehash
  return 1	# Because we didn't really complete anything
}

zstyle ':completion::complete:*' use-cache 1
# case insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*' group-name ''
#zstyle ':completion:*' completer _oldlist _expand _force_rehash _complete
zstyle ':completion:*' completer _expand _force_rehash _complete _approximate _ignored

# generate descriptions with magic.
zstyle ':completion:*' auto-description 'specify: %d'

# Don't prompt for a huge list, page it!
zstyle ':completion:*:default' list-prompt '%S%M matches%s'

# Don't prompt for a huge list, menu it!
zstyle ':completion:*:default' menu 'select=0'

# Have the newer files last so I see them first
zstyle ':completion:*' file-sort modification reverse

# color code completion!!!!  Wohoo!
zstyle ':completion:*' list-colors "=(#b) #([0-9]#)*=36=31"

# Separate man page sections. Neat.
zstyle ':completion:*:manuals' separate-sections true

# Choose something pretty
zstyle ':completion:*' list-separator ' → '

# complete with a menu for xwindow ids
zstyle ':completion:*:windows' menu on=0
zstyle ':completion:*:expand:*' tag-order all-expansions

# more errors allowed for large words and fewer for small words
zstyle ':completion:*:approximate:*' max-errors 'reply=(  $((  ($#PREFIX+$#SUFFIX)/3  ))  )'

# Errors format
zstyle ':completion:*:corrections' format '%B%d (errors %e)%b'

# Don't complete stuff already on the line
zstyle ':completion::*:(rm|vi):*' ignore-line true

# Don't complete directory we are already in (../here)
zstyle ':completion:*' ignore-parents parent pwd

zstyle ':completion::approximate*:*' prefix-needed false
# }}}


# Terminal - terminfo, base16 colors, iTerm integration {{{
DOT_TI="$HOME"'/.terminfo/'"$TERM"'.ti'
[ -f "$DOT_TI" ] \
    || infocmp "$TERM" \
    | sed 's/kbs=^[hH]/kbs=\\177/' \
    > "$DOT_TI"
tic "$DOT_TI"

if [ "$OSX" = "$TRUE" ]; then
    # base16-shell
    export BASE16_SHELL="$HOME/.config/base16-shell"
    if ((${#ITERM_DOTAPP[@]})); then
        [ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"
        # make `neovim` and `tmux` play nice
        #   https://iterm2.com/documentation-shell-integration.html
        # tmux.plugin settings
        #   https://github.com/robbyrussell/oh-my-zsh/blob/master/plugins/tmux/tmux.plugin.zsh
        export ZSH_TMUX_FIXTERM=true
        #export ZSH_TMUX_ITERM2="$(((${#ITERM_DOTAPP[@]})) && echo true || echo false)"
        #export ZSH_TMUX_FIXTERM_WITH_256COLOR="screen-256color"
        iterm2_integration="$HOME"'/.iterm2_shell_integration.'"$SHELL_NAME"
        [[ -f "$iterm2_integration" ]] && source "$iterm2_integration"
    fi
fi
# }}}


# Plugins. {{{
export ANTIGEN_HS_SANDBOX="cabal"
source ~/.zsh/antigen-hs/init.zsh
autoload -U compinit && compinit
# }}}


# Key bindings {{{
bindkey -M viins 'jj'              vi-cmd-mode
bindkey -M viins 'kk' last-cmd-and-vi-cmd-mode

# useful motion shortcuts
bindkey -M viins 'HH' beginning-of-line
bindkey -M viins 'LL'       end-of-line
bindkey -M vicmd 'HH' beginning-of-line
bindkey -M vicmd 'LL'       end-of-line

# <PageUp>, <PageDown>
bindkey          '^[[5~' kill-word
bindkey          '^[[5~' kill-word
bindkey -M vicmd '^[[6~' kill-word

# <Backspace>
bindkey          '^?' backward-delete-char
bindkey          '^h' backward-delete-char
# <S-Backspace>
bindkey          '^[[3~'       delete-char
bindkey -M vicmd '^[[3~'       delete-char
# <C-Backspace>
bindkey          '^w' backward-kill-word
# <C-S-Backspace>
bindkey          '^[[2J'       kill-word
bindkey -M vicmd '^[[2J'       kill-word

# <Up>, <Down>
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# <Home>, <End>
bindkey '^[[H' beginning-of-line
bindkey '^[[F'       end-of-line

# <Option-Left>, <Option-Right>
bindkey -M viins '^[b'    backward-word
bindkey -M viins '^[f'     forward-word
bindkey -M vicmd '^[b' vi-backward-word
bindkey -M vicmd '^[f'  vi-forward-word

# <Command-Backspace>
bindkey -M viins '^U' backward-kill-line
bindkey -M vicmd '^U' backward-kill-line

# zsh vi-mode fixes
bindkey -M viins ' ' magic-space
function _last-cmd-and-vi-cmd-mode {
    zle vi-cmd-mode
    zle history-substring-search-up
}
zle -N last-cmd-and-vi-cmd-mode _last-cmd-and-vi-cmd-mode
#ls}}}


# Syntax highlighting. {{{ Remember to install `zsh-syntax-highlighting`.
if (($+ZSH_HIGHLIGHT_HIGHLIGHTERS)); then
    typeset -A ZSH_HIGHLIGHT_STYLES
    ZSH_HIGHLIGHT_PATTERNS+=('rm -*' fg=grey,bold,underline,bg=red)
    ZSH_HIGHLIGHT_PATTERNS+=('sudo*' fg=white,bold,bg=red)
    #ZSH_HIGHLIGHT_STYLES[default]=none
    #ZSH_HIGHLIGHT_STYLES[unknown-token]=fg=248
    ZSH_HIGHLIGHT_STYLES[reserved-word]=fg=009,standout
    ZSH_HIGHLIGHT_STYLES[alias]=fg=cyan,bold
    ZSH_HIGHLIGHT_STYLES[builtin]=fg=145,bold,underline
    ZSH_HIGHLIGHT_STYLES[function]=fg=cyan,bold
    ZSH_HIGHLIGHT_STYLES[command]=fg=white,bold
    #ZSH_HIGHLIGHT_STYLES[precommand]=fg=white,underline
    #ZSH_HIGHLIGHT_STYLES[commandseparator]=none
    #ZSH_HIGHLIGHT_STYLES[hashed-command]=fg=009
    ZSH_HIGHLIGHT_STYLES[path]=fg=214,underline
    ZSH_HIGHLIGHT_STYLES[globbing]=fg=063
    ZSH_HIGHLIGHT_STYLES[history-expansion]=fg=white,underline
    ZSH_HIGHLIGHT_STYLES[single-hyphen-option]=fg=137,bold
    ZSH_HIGHLIGHT_STYLES[double-hyphen-option]=fg=244
    #ZSH_HIGHLIGHT_STYLES[back-quoted-argument]=none
    ZSH_HIGHLIGHT_STYLES[single-quoted-argument]=fg=137,bold
    ZSH_HIGHLIGHT_STYLES[double-quoted-argument]=fg=244
    ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]=fg=148,bold
    #ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]=fg=009
    #ZSH_HIGHLIGHT_STYLES[assign]=none
fi
# }}}


# Aliases. {{{
. "$DOTFILES"/.aliases
aliasof() {
    if [ "$OSX" = "$TRUE" ]; then
        sed_flags='-E'
    else
        sed_flags='-r'
    fi
    alias $1 | sed $sed_flags "s|$1=(.*)|\1|" | sed $sed_flags "s|'||g"
}
# }}}


# Options. setopt {{{ NOTE - run me last
setopt MULTIOS
setopt CDABLEVARS
setopt PROMPT_SUBST
setopt COMPLETE_ALIASES

setopt IGNORE_EOF

setopt GLOB_COMPLETE
setopt EXTENDED_GLOB
setopt NO_CASE_GLOB
setopt NUMERIC_GLOB_SORT

setopt HIST_IGNORE_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_SAVE_NO_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_SPACE
setopt EXTENDED_HISTORY

setopt AUTO_PUSHD
setopt PUSHD_MINUS
setopt PUSHD_TO_HOME
setopt PUSHD_IGNORE_DUPS

setopt RC_EXPAND_PARAM

setopt RM_STAR_WAIT

setopt TRANSIENT_RPROMPT
# }}}
