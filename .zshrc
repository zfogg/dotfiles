#!/usr/bin/env zsh
# vim: set fdm=marker:


# Prompt. {{{
TRAPWINCH() {
  zle && { zle reset-prompt; zle -R }
}
# }}}


# Modules. {{{
autoload -U colors && colors
autoload -U zsh-mime-setup && zsh-mime-setup

zmodload zsh/complist
GENCOMPL_FPATH="$HOME/.zsh/complete"
source $HOME/.zsh/zsh-completion-generator/zsh-completion-generator.plugin.zsh
autoload -U compinit && compinit

autoload -U edit-command-line && zle -N edit-command-line
autoload -U url-quote-magic && zle -N self-insert url-quote-magic
autoload -U select-word-style && select-word-style bash
# }}}


# Autocomplete. {{{
unsetopt menu_complete

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

unsetopt LIST_AMBIGUOUS
setopt  COMPLETE_IN_WORD

# Separate man page sections.  Neat.
zstyle ':completion:*:manuals' separate-sections true

# Egomaniac!
zstyle ':completion:*' list-separator 'fREW'

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


# Environment variables. {{{

#export TERM="$TERM"
export COLORTERM="$TERM"

export OSX="`[ $(uname) = "Darwin" ] && echo $?`"

export EDITOR="$(which nvim)"

export VISUAL="$(which nvim)"

export PAGER="$(which vimpager)"

export LESS="-R"

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export GOPATH="$HOME/src/go"
export GOROOT="`go env GOROOT`"

export AWS_DEFAULT_PROFILE="zfogg-zfogg"

export HOMEBREW_GITHUB_API_TOKEN='5c8597261ad87cf1951e4e4ce9e9c3b1d1b361be'

export WORKON_HOME="~/.virtualenvs"

export POWERLINE_CONFIG_COMMAND="python /usr/local/bin/powerline-config"

export ZSH_TMUX_FIXTERM=true
export ZSH_TMUX_ITERM2=true
export ZSH_TMUX_FIXTERM_WITHOUT_256COLOR="xterm"
export ZSH_TMUX_FIXTERM_WITH_256COLOR="xterm-256color"
# }}}


# $PATH, $MANPATH, $FPATH {{{
typeset -U path
typeset -U fpath
typeset -U manpath

path=(
    ~/bin
    ~/.cabal/bin
    ~/.cargo/bin
    $GOROOT/bin
    $GOPATH/bin
    /usr/local/bin
    $path[@]
)

manpath=(
    $manpath[@]
)

fpath=(
    /usr/local/share/zsh/site-functions
    /usr/local/share/zsh/functions
    $fpath[@]
)

if [ "$OSX" ]; then
    path=(
        /usr/local/opt/coreutils/libexec/gnubin
        /usr/local/opt/gnu-sed/bin
        /usr/local/opt/gnu-tar/bin
        /usr/local/opt/gnu-indent/bin
        $path[@]
    )
    manpath=(
        /usr/local/opt/coreutils/libexec/gnuman
        /usr/local/opt/gnu-sed/share
        /usr/local/opt/gnu-tar/share
        /usr/local/opt/gnu-indent/share
        $manpath[@]
    )
fi

path=($^path(N))
manpath=($^manpath(N))
fpath=($^fpath(N))
# }}}


# Aliases. {{{
. ~/.aliases

function aliasof {
    if [ "$OSX" ]; then
        sed_flags='-E'
    else
        sed_flags='-r'
    fi
    alias $1 | sed $sed_flags "s|$1=(.*)|\1|" | sed $sed_flags "s|'||g"
}
# }}}


# Plugins. {{{
export ANTIGEN_HS_SANDBOX="cabal"
source ~/.zsh/antigen-hs/init.zsh
# }}}


# Key bindings. {{{
function _last-cmd-and-vi-cmd-mode {
    zle vi-cmd-mode
    zle history-substring-search-up
}
zle -N last-cmd-and-vi-cmd-mode _last-cmd-and-vi-cmd-mode

bindkey -M viins ' ' magic-space
bindkey -M viins 'jj' vi-cmd-mode
bindkey -M viins 'kk' last-cmd-and-vi-cmd-mode
bindkey -M viins 'HH' beginning-of-line
bindkey -M viins 'LL' end-of-line

# Ctrl-backspace and ctrl-delete.
bindkey '^[[2J' kill-word          # But you should use <a-d>.
bindkey '^H'    backward-kill-word # <c-bs>.

# Up and down.
if [ "$OSX" ]; then
    bindkey '^[OA' history-substring-search-up
    bindkey '^[OB' history-substring-search-down
else
    bindkey '^[[A' history-substring-search-up
    bindkey '^[[B' history-substring-search-down
fi

# Home and end.
bindkey '\e[1~' beginning-of-line
bindkey '\e[4~' end-of-line
# }}}


# theme {{{
BASE16_SCHEME="default-dark"
BASE16_SHELL="$HOME/.config/base16-shell/scripts/base16-$BASE16_SCHEME.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

if [ "$OSX" ]; then
    # https://iterm2.com/documentation-shell-integration.html
    source ~/.iterm2_shell_integration."`basename "$SHELL"`"
    # neovim
    [[ ! -f ~/".$TERM".ti ]] || infocmp "$TERM" | sed 's/kbs=^[hH]/kbs=\\177/' > ~/."$TERM".ti
    tic ~/".$TERM".ti
fi

eval "$(dircolors ~/.dircolors)"
export LSCOLORS="$LS_COLORS"

# Syntax highlighting. {{{ Remember to install `zsh-syntax-highlighting`.
if (($+ZSH_HIGHLIGHT_HIGHLIGHTERS)); then
    ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor root)
    ZSH_HIGHLIGHT_PATTERNS+=('rm -*' fg=grey,bold,underline,bg=red)
    ZSH_HIGHLIGHT_PATTERNS+=('sudo*'   fg=white,bold,bg=red)
    ZSH_HIGHLIGHT_STYLES[default]=none
    ZSH_HIGHLIGHT_STYLES[unknown-token]=fg=248
    #ZSH_HIGHLIGHT_STYLES[reserved-word]=fg=009,standout
    ZSH_HIGHLIGHT_STYLES[alias]=fg=cyan,bold
    ZSH_HIGHLIGHT_STYLES[builtin]=fg=145,bold,underline
    ZSH_HIGHLIGHT_STYLES[function]=fg=cyan,bold
    ZSH_HIGHLIGHT_STYLES[command]=fg=white,bold
    ZSH_HIGHLIGHT_STYLES[precommand]=fg=white,underline
    #ZSH_HIGHLIGHT_STYLES[commandseparator]=none
    #ZSH_HIGHLIGHT_STYLES[hashed-command]=fg=009
    ZSH_HIGHLIGHT_STYLES[path]=fg=214,underline
    ZSH_HIGHLIGHT_STYLES[globbing]=fg=063
    ZSH_HIGHLIGHT_STYLES[history-expansion]=fg=white,underline
    ZSH_HIGHLIGHT_STYLES[single-hyphen-option]=none
    ZSH_HIGHLIGHT_STYLES[double-hyphen-option]=none
    ZSH_HIGHLIGHT_STYLES[back-quoted-argument]=none
    ZSH_HIGHLIGHT_STYLES[single-quoted-argument]=fg=063
    ZSH_HIGHLIGHT_STYLES[double-quoted-argument]=fg=063
    ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]=fg=009
    ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]=fg=009
    #ZSH_HIGHLIGHT_STYLES[assign]=none
fi
# }}}
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

