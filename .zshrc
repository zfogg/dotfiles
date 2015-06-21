#!/usr/bin/env zsh
# vim: set fdm=marker:


# unalias {{{
unalias ls  2> /dev/null
unalias cat 2> /dev/null
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


# Automatic completion. {{{
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


# antigen-hs {{{

# liquidprompt
LP_ENABLE_TIME=1
LP_USER_ALWAYS=1

typeset -U fpath
fpath=(
    /usr/local/share/zsh/site-functions
    /usr/local/share/zsh/functions
    $fpath[@]
)
fpath=($^fpath(N))

source ~/.zsh/antigen-hs/init.zsh

# }}}


# $PATH directories. {{{
typeset -U path
path=(
    ~/bin
    ~/.cabal/bin
    $GOROOT/bin
    $path[@]
)

typeset -U manpath
manpath=(
    $manpath[@]
)

if [ "$OSX" ]; then
    path=(
        /usr/local/bin
        /usr/local/opt/make/bin
        /usr/local/opt/coreutils/libexec/gnubin
        $GEM_HOME/bin
        ~/Library/Python/2.7/bin
        $path[@]
    )

    manpath=(
        /usr/local/opt/coreutils/libexec/gnuman
        $manpath[@]
    )
fi

path=($^path(N))
manpath=($^manpath(N))
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
[[ -f ~/$TERM.ti ]] && tic ~/$TERM.ti

