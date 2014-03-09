#!/usr/bin/env zsh
# vim: set fdm=marker:


# z.sh {{{
. ~/.z.sh/z.sh
# }}}


# base16-shell {{{
BASE16_SCHEME="default"
BASE16_SHELL="$HOME/.config/base16-shell/base16-$BASE16_SCHEME.dark.sh"
[[ -s $BASE16_SHELL ]] && . $BASE16_SHELL
# }}}


# Options. {{{
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

setopt VI
# }}}


# Prompt. {{{
# }}}


# Modules. {{{
autoload -U colors compinit zcalc zsh-mime-setup

colors
compinit
zsh-mime-setup
# }}}


# Automatic completion. {{{
zstyle ':completion:*:functions' ignored-patterns '_*'
# }}}


# Key bindings. {{{
# vi-mode.
function _last-cmd-and-vi-cmd-mode {
    vi-cmd-mode
    history-substring-search-up
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
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# Home and end.
bindkey '\e[1~' beginning-of-line
bindkey '\e[4~' end-of-line
# }}}


# $PATH directories. {{{
typeset -U path

path=(~/bin "$path[@]")

path=(~/sbin "$path[@]")

path=(~/usr/bin "$path[@]")

path=(~/usr/sbin "$path[@]")

path=(~/.cabal/bin "$path[@]")

path=(~/.gem/ruby/2.1.0/bin "$path[@]")

path=(/usr/lib/colorgcc/bin "$path[@]")

path=(~/pebble/bin "$path[@]")

path=($^path(N))
# }}}


# Environment variables. {{{
if [ "$TERM" != "st-256color" ]; then
    export TERM=xterm-256color
fi

export EDITOR=vim

export VISUAL=vim

export PAGER=vimpager

export LESS='-R'

eval $(dircolors -b) # Generate and set `$LS_COLORS`.

# Enable video acceleration.
export LIBVA_DRIVER_NAME=vdpau
export VDPAU_DRIVER=r600

export PYTHON=`which python2`

export GEM_HOME=~/.gem/ruby/2.1.0

export CHROME_BIN=`which chromium`
# }}}


# Aliases. {{{
. ~/.aliases
# }}}


# Et cetera. {{{
# Load aur/command-not-found.
[ -r /etc/profile.d/cnf.sh ] && . /etc/profile.d/cnf.sh
# }}}


# oh-my-zsh {{{
ZSH=~/.oh-my-zsh

ZSH_THEME=sporty_256

DISABLE_AUTO_UPDATE=true

COMPLETION_WAITING_DOTS=true

DISABLE_CORRECTION=true

DISABLE_UNTRACKED_FILES_DIRTY=true

plugins=(archlinux bower battery cabal cake coffee colored-man colorize cp
         custom-aliases django extract gem gitfast git-extras git-flow go
         heroku history-substring-search lein node npm pip python redis-cli
         svn systemd tmux themes urltools vi-mode web-search
         zsh-syntax-highlighting)

. $ZSH/oh-my-zsh.sh
# }}}


# Syntax highlighting. {{{
if (($+ZSH_HIGHLIGHT_HIGHLIGHTERS)); then
    # aur/zsh-syntax-highlighting
    #   # Remember to make a symlink:
    #   $ ln -s /usr/share/zsh/plugins/zsh-syntax-highlighting \
    #           ~/.oh-my-zsh/plugins/zsh-syntax-highlighting

    ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor root)

    ZSH_HIGHLIGHT_PATTERNS+=('rm -rf *' fg=black,bold,bg=red)
    ZSH_HIGHLIGHT_PATTERNS+=('sudo *'   fg=white,bold,bg=red)

    ZSH_HIGHLIGHT_STYLES[default]=none
    ZSH_HIGHLIGHT_STYLES[unknown-token]=fg=009
    ZSH_HIGHLIGHT_STYLES[reserved-word]=fg=009,standout
    ZSH_HIGHLIGHT_STYLES[alias]=fg=cyan,bold
    ZSH_HIGHLIGHT_STYLES[builtin]=fg=white,bold,bg=yellow
    ZSH_HIGHLIGHT_STYLES[function]=fg=white,bold,bg=cyan
    ZSH_HIGHLIGHT_STYLES[command]=fg=white,bold
    ZSH_HIGHLIGHT_STYLES[precommand]=fg=white,underline
    ZSH_HIGHLIGHT_STYLES[commandseparator]=none
    ZSH_HIGHLIGHT_STYLES[hashed-command]=fg=009
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


PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
