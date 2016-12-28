#!/usr/bin/env zsh
# vim: set fdm=marker:


# modules {{{
zmodload zsh/zpty
autoload -Uz                 \
    url-quote-magic          \
    colors                   \
    select-word-style        \
    zsh-mime-setup           \
    compinit
    #predict-on               \
colors
select-word-style normal
zsh-mime-setup
#compinit -i -d ~/.zcompdump # this gets done after fpath
#predict-on
# }}}


# autocomplete {{{
source $HOME/.zsh/zsh-completion-generator/zsh-completion-generator.plugin.zsh

unsetopt MENU_COMPLETE
unsetopt LIST_AMBIGUOUS
setopt   COMPLETE_IN_WORD

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
zstyle ':completion::*:(rm|v):*' ignore-line true
# Don't complete directory we are already in (../here)
zstyle ':completion:*' ignore-parents parent pwd
zstyle ':completion::approximate*:*' prefix-needed false
# https://github.com/robbyrussell/oh-my-zsh/blob/master/plugins/ssh-agent/ssh-agent.plugin.zsh
zstyle ':omz:plugins:ssh-agent' agent-forwarding on

set completion-ignore-case on
set show-all-if-ambiguous on
# }}}


# terminal - terminfo, base16 colors, iTerm integration {{{
DOT_TI="$HOME"'/.terminfo/'"$TERM"'.ti'
[[ -f "$DOT_TI" ]] \
    || infocmp "$TERM" \
    | sed 's/kbs=^[hH]/kbs=\\177/' \
    > "$DOT_TI"
tic "$DOT_TI"

if [[ "$OSX" == "$TRUE" ]]; then
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


# plugins {{{
export ANTIGEN_HS_SANDBOX="cabal"

[ -f "$BREW"/etc/profile.d/z.sh ] && source "$BREW"/etc/profile.d/z.sh
[ -f ~/.fzf.zsh                 ] && source ~/.fzf.zsh

_my_plugins=(
    battery
    branch
    brew
    #brew-cask
    #colored-man-pages
    compleat
    extract
    emoji
    git
    gitfast
    gitignore
    git-extras
    gnu-utils
    golang
    httpie
    node
    osx
    ruby
    rust
    #safe-paste
    ssh-agent
    sudo
    tmux
    vi-mode
    xcode
    #zsh-navigation-tools
)

_load_plugin() {
    local plugin="${1:-$0}"
    [[ "$plugin" == "$0" ]] \
        && >&2 echo "$0"' - missing required arg(s):' \
        && >&2 echo '\t''arg1 = plugin to load' \
        && return 1

    local plugin_d="$HOME"/.zsh/.oh-my-zsh/plugins/"$1"
    [[ ! -d "$plugin_d" ]] \
        && >&2 echo "$0"' - inaccessible plugin directory:' \
        && >&2 echo '\t''$plugin_d = '"$plugin_d" \
        && return 2

    local plugin_dotplugin="$plugin_d"/"$1".plugin.zsh
    local plugin_completion="$plugin_d"/_"$1"
    [[ ! -f "$plugin_dotplugin" && ! -f "$plugin_completion" ]] \
        && >&2 echo "$0"' - inaccessible files: '"$1"'.plugin.zsh and _'"$1"':' \
        && >&2 echo '\t''$plugin_dotplugin = '"$plugin_dotplugin" \
        && return 3

    local should_source="${2:-1}"
    [[ "$should_source" == "1" && -f "$plugin_dotplugin" ]] \
        && source "`realpath "$plugin_dotplugin"`"

    local should_fpath="${3:-1}"
    #[[ "$should_fpath"  == "1" && -f "$plugin_completion" ]] \
    [[ "$should_fpath"  == "1" ]] \
        && fpath+="`realpath "$plugin_d"`"
}
for p in $_my_plugins; do
    _load_plugin "$p" 0 1
done
compinit -i -d ~/.zcompdump."$USER"."$SHELL"
for p in $_my_plugins; do
    _load_plugin "$p" 1 0
done

unset _my_plugins
unfunction _load_plugin

# antigen.hs
source ~/.zsh/antigen-hs/init.zsh
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

# generic colourizer
source $BREW/etc/grc.bashrc
# }}}


# key bindings {{{
bindkey -M viins 'jj'              vi-cmd-mode
bindkey -M viins 'kk' last-cmd-and-vi-cmd-mode

bindkey -M viins 'jj'              vi-cmd-mode
bindkey -M viins 'kk' last-cmd-and-vi-cmd-mode

bindkey -rM vicmd ':'
bindkey  -M vicmd ':' execute-named-cmd # ; line vim

# useful motion shortcuts
bindkey -M viins 'HH'    beginning-of-line
bindkey -M viins 'LL'          end-of-line
bindkey -M vicmd 'HH' vi-beginning-of-line
bindkey -M vicmd 'LL'       vi-end-of-line

bindkey -r        '^H'
bindkey -rM viins '^H'
bindkey -rM vicmd '^H'
bindkey -r        '^J'
bindkey -rM viins '^J'
bindkey -rM vicmd '^J'
bindkey -r        '^L'
bindkey -rM viins '^L'
bindkey -rM vicmd '^L'

bindkey          '^[[Z' reverse-menu-complete

# <M-direction>
bindkey          '^[[1;3D'    backward-word
bindkey -M vicmd '^[[1;3D' vi-backward-word
bindkey -M viins '^[[1;3D'    backward-word
bindkey          '^[[1;3C'     forward-word
bindkey -M vicmd '^[[1;3C'  vi-forward-word
bindkey -M viins '^[[1;3C'     forward-word

# <S-Backspace>
bindkey          '^[[3~'       delete-char
bindkey -M vicmd '^[[3~'    vi-delete-char
# <C-Backspace>
bindkey          '^W'    backward-kill-word
bindkey -M viins '^W'    backward-kill-word
bindkey -M vicmd '^W' vi-backward-kill-word
# <C-S-Backspace>
bindkey          '^[[2J'            kill-word
bindkey -M vicmd '^[[2J' vi-forward-kill-word
# <Command-Backspace>
bindkey            '^U' backward-kill-line
bindkey -M vicmd   '^U' backward-kill-line
bindkey -M viins   '^U' vi-kill-line
bindkey            '^K'    kill-line
bindkey -M vicmd   '^K' vi-kill-eol
bindkey -M viins   '^K' vi-change-eol

# <Up>, <Down>
bindkey          '^[[A' history-substring-search-up
bindkey          '^[[B' history-substring-search-down
bindkey          '^[[a' history-beginning-search-backward
bindkey          '^[[b' history-beginning-search-forward

# <Home>, <End>
bindkey '^[[H'  beginning-of-line
bindkey '^[[F'        end-of-line
bindkey '^[[5~'   up-line-or-history
bindkey '^[[6~' down-line-or-history

# <Option-Left>, <Option-Right>
bindkey -M viins '^[b'    backward-word
bindkey -M viins '^[f'     forward-word
bindkey -M vicmd '^[b' vi-backward-word
bindkey -M vicmd '^[f'  vi-forward-word

# zsh vi-mode fixes
zle -N self-insert url-quote-magic
bindkey -M viins ' '   magic-space

function _last-cmd-and-vi-cmd-mode {
    zle vi-cmd-mode
    zle history-substring-search-up
}
zle -N last-cmd-and-vi-cmd-mode _last-cmd-and-vi-cmd-mode

# zsh-users/zsh-autosuggestions
bindkey -M viins '^ '   autosuggest-accept
bindkey -M viins '^[[c' autosuggest-execute

# edit-command-line
function _autosuggest-accept-and-edit-command-line {
    zle autosuggest-accept
    zle edit-command-line
}
zle -N autosuggest-accept-and-edit-command-line _autosuggest-accept-and-edit-command-line
autoload -z edit-command-line
zle -N edit-command-line
bindkey '^[[v' edit-command-line
bindkey '^[[V' autosuggest-accept-and-edit-command-line
#ls}}}


# syntax highlighting {{{ Remember to install `zsh-syntax-highlighting`.
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


# aliases {{{
. "$DOTFILES"/.aliases
aliasof() {
    if [[ "$OSX" == "$TRUE" ]]; then
        sed_flags='-E'
    else
        sed_flags='-r'
    fi
    alias $1 | sed $sed_flags "s|$1=(.*)|\1|" | sed $sed_flags "s|'||g"
}
# }}}


# options - setopt {{{ NOTE - run me last
setopt MULTIOS
setopt CDABLEVARS

setopt noCOMPLETE_ALIASES
setopt COMPLETE_IN_WORD

setopt PROMPT_SUBST
setopt TRANSIENT_RPROMPT


setopt IGNORE_EOF

setopt GLOB_COMPLETE
setopt EXTENDED_GLOB
setopt NO_CASE_GLOB
setopt NUMERIC_GLOB_SORT

setopt APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS
setopt HIST_SAVE_NO_DUPS
setopt INC_APPEND_HISTORY SHARE_HISTORY

setopt AUTO_PUSHD
setopt PUSHD_MINUS
setopt PUSHD_TO_HOME
setopt PUSHD_IGNORE_DUPS

setopt RC_EXPAND_PARAM

setopt RM_STAR_WAIT
# }}}
