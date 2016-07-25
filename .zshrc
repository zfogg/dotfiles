#!/usr/bin/env zsh
# vim: set fdm=marker:


# Homebrew {{{
export HOMEBREW_PREFIX='/usr/local'
export HOMEBREW_GITHUB_API_TOKEN='5c8597261ad87cf1951e4e4ce9e9c3b1d1b361be'
unalias run-help
autoload run-help
export HELPDIR="${HOMEBREW_PREFIX}/share/zsh/help"
#}}}


# Prompt. {{{
TRAPWINCH() {
  zle && { zle reset-prompt; zle -R }
}
# }}}


# Modules. {{{
#autoload -U colors && colors
autoload -U zsh-mime-setup && zsh-mime-setup
autoload -U edit-command-line && zle -N edit-command-line
autoload -U url-quote-magic && zle -N self-insert url-quote-magic
autoload -U select-word-style && select-word-style bash
# }}}


# Autocomplete. {{{
GENCOMPL_FPATH="$HOME/.zsh/complete"
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

# Separate man page sections.  Neat.
zstyle ':completion:*:manuals' separate-sections true

# Egomaniac!
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


# Environment variables. {{{
#export TERM="$TERM"
export COLORTERM="$TERM"

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export EDITOR="$(which nvim)"
export VISUAL="$(which nvim)"
export PAGER="$(which vimpager)"
export LESS="-isMR"

export TRUE=true
export FALSE=false

export OSX="$(
    [ "`uname`" = "Darwin" ]
    [ $? -eq 0 ] \
        && echo "$TRUE" \
        || echo "$FALSE"
)"
export SHELL_NAME="$(
    [ "$OSX" = "$TRUE" ] \
        && ps -p$$ -ocommand= | tr -d '-' \
        || ps -p$$ -ocmd=
)"
export SHELL_NAME_U="$SHELL_NAME:u"

export GOPATH="$HOME/src/go"
export GOROOT="$HOMEBREW_PREFIX/opt/go/libexec"

export WORKON_HOME="~/.virtualenvs"

export POWERLINE_CONFIG_COMMAND="python $HOMEBREW_PREFIX/bin/powerline-config"

# path, manpath, fpath {{{
typeset -gU  path     PATH
typeset -gU  fpath    FPATH
typeset -gU  manpath  MANPATH
# new vars
typeset -gU  infopath INFOPATH
typeset -gTU INFOPATH infopath

path=(
    ~/bin
    ~/.cabal/bin
    ~/.cargo/bin
    $GOROOT/bin
    $GOPATH/bin
    $HOMEBREW_PREFIX/bin
    $(command -p getconf PATH | tr ':' '\n')
    $path
); path=($^path(N-/))

fpath=(
    $HOMEBREW_PREFIX/share/zsh-completions
    $HOMEBREW_PREFIX/share/zsh/site-functions
    $HOMEBREW_PREFIX/share/zsh/functions
    $fpath
); fpath=($^fpath(N-/))

manpath=(
    $HOMEBREW_PREFIX/share/man
    /usr/share/man
    $manpath
); manpath=($^manpath(N-/))

infopath=(
    $HOMEBREW_PREFIX/share/info
    /usr/share/info
    $infopath
); infopath=($^infopath(N-/))

if [ "$OSX" ]; then
    path=(
        $HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin
        $HOMEBREW_PREFIX/opt/gnu-sed/bin
        $HOMEBREW_PREFIX/opt/gnu-tar/bin
        $HOMEBREW_PREFIX/opt/gnu-indent/bin
        $path
    ); path=($^path(N-/))
    manpath=(
        $HOMEBREW_PREFIX/opt/coreutils/share/man
        $HOMEBREW_PREFIX/opt/gnu-sed/share/man
        $HOMEBREW_PREFIX/opt/gnu-tar/share/man
        $HOMEBREW_PREFIX/opt/gnu-indent/share/man
        $manpath
    ); manpath=($^manpath(N-/))
    infopath=(
        $HOMEBREW_PREFIX/opt/coreutils/share/info
        $HOMEBREW_PREFIX/opt/gnu-sed/share/info
        $HOMEBREW_PREFIX/opt/gnu-tar/share/info
        $HOMEBREW_PREFIX/opt/gnu-indent/share/info
        $infopath
    ); infopath=($^infopath(N-/))
fi
# }}}

export ZSH_TMUX_FIXTERM=true
export ZSH_TMUX_ITERM2=true
export ZSH_TMUX_FIXTERM_WITHOUT_256COLOR="screen"
export ZSH_TMUX_FIXTERM_WITH_256COLOR="$ZSH_TMUX_FIXTERM_WITHOUT_256COLOR"'-256color'
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
if [ "$OSX" = "$TRUE" ]; then
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


# Shell theme and colors {{{
BASE16_SCHEME="default-dark"
BASE16_SHELL="$HOME"'/.config/base16-shell/scripts/base16-'"$BASE16_SCHEME"'.sh'
[[ -s "$BASE16_SHELL" ]] && source "$BASE16_SHELL"

if [ "$OSX" = "$TRUE" ]; then
    # https://iterm2.com/documentation-shell-integration.html
    source "$HOME"'/.iterm2_shell_integration.'"$SHELL_NAME"
    # make `neovim` and `tmux` play nice
    DOT_TI="$HOME"'/.'"$TERM"'.ti' 
    [ -f "$DOT_TI" ] \
        || infocmp "$TERM" \
        | sed 's/kbs=^[hH]/kbs=\\177/' \
        > "$DOT_TI"
    tic "$DOT_TI"
    unset DOT_TI
fi

eval "$(dircolors ~/.dircolors)"
export CLICOLOR="YES"
export LSCOLORS="$LS_COLORS"
# }}}


# Plugins. {{{
export ANTIGEN_HS_SANDBOX="cabal"
source ~/.zsh/antigen-hs/init.zsh
autoload -U compinit && compinit
# }}}


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
. ~/.aliases
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
