#!/usr/bin/env zsh
# vim: set fdm=marker:


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


# Environment variables. {{{

export WI_HOME=~/src/wi

export OSX=`[ $(uname) = "Darwin" ] && echo $?`

[ "$TERM" != "st-256color" ] && export TERM="screen-256color"

export EDITOR=vi

export VISUAL=vi

export PAGER=vimpager

export LESS='-R'

if [ "$OSX" ]; then
    export VIM_APP_DIR="/Applications/MacVim"
    export PYTHON=/usr/local/bin/python
    export LC_ALL=en_US.UTF-8
    export LANG=en_US.UTF-8
    export ARCHFLAGS="-arch x86_64"
else
    eval $(dircolors -b) # Generate and set `$LS_COLORS`.
    export PYTHON=`which python2`
fi

export GEM_HOME=~/.gem/ruby/2.1.0
export RAILS_ENV=development

export CHROME_BIN=`which chromium`

export GOPATH="$HOME/src/go"
export GOROOT=`go env GOROOT`

export AWS_DEFAULT_PROFILE=zfogg-zfogg

export JAVA_HOME=`/usr/libexec/java_home`

# }}}


# Key bindings. {{{
# vi-mode.
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


# Et cetera. {{{


# $PATH directories. {{{
typeset -U path
typeset -U manpath

path=(~/bin "$path[@]")

path=(~/.cabal/bin "$path[@]")

path=(~/.gem/ruby/2.1.0/bin "$path[@]")

path=(~/Library/Python/2.7/bin "$path[@]")

path=("$GOROOT/bin" "$path[@]")


if [ "$OSX" ]; then
    path=(/usr/local/bin "$path[@]")

    path=(/usr/local/opt/make/bin "$path[@]")

    path=(/usr/local/opt/coreutils/libexec/gnubin "$path[@]")
    manpath=(/usr/local/opt/coreutils/libexec/gnuman "$manpath[@]")
fi

path=($^path(N))
manpath=($^manpath(N))
# }}}


# antigen-hs {{{

# liquidprompt
LP_ENABLE_TIME=1
LP_USER_ALWAYS=1

source ~/.zsh/antigen-hs/init.zsh

# }}}


# Syntax highlighting. {{{
# Remember to install `zsh-syntax-highlighting`.
if (($+ZSH_HIGHLIGHT_HIGHLIGHTERS)); then
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


# Extra init scripts. {{{
#source ~/.autoenv/activate.sh
#source /usr/local/bin/virtualenvwrapper.sh
source ~/.profile
export NVM_DIR=~/.nvm
#source $(brew --prefix nvm)/nvm.sh
# }}}


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


# For neovim.
tic ~/$TERM.ti

