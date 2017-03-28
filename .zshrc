#!/usr/bin/env zsh
# vim: set fdm=marker:


# note: meta helpers {{{
function command_exists() { command -v "$1" 2>/dev/null 1>&2 }
function alias_exists()   { alias      "$1" 2>/dev/null 1>&2 }
# }}}


# zsh modules {{{
zmodload zsh/zpty
autoload -Uz          \
    url-quote-magic   \
    colors            \
    select-word-style \
    zsh-mime-setup    \
    predict-on        \
colors
select-word-style normal
zsh-mime-setup
predict-on
# }}}


# autocomplete {{{
source ~/.zsh/zsh-completion-generator/zsh-completion-generator.plugin.zsh

unsetopt MENU_COMPLETE
unsetopt LIST_AMBIGUOUS
setopt   COMPLETE_IN_WORD

_force_rehash() {
  (( CURRENT == 1 )) && rehash
  return 1  # Because we didn't really complete anything
}

zstyle ':completion:*'               special-dirs      true
zstyle ':completion::complete:*'     use-cache         1
# case insensitive completion
zstyle ':completion:*'               matcher-list      'm:{a-z}={a-z}'
zstyle ':completion:*'               verbose           yes
zstyle ':completion:*:descriptions'  format            '%b%d%b'
zstyle ':completion:*:messages'      format            '%d'
zstyle ':completion:*:warnings'      format            'no matches for: %d'
zstyle ':completion:*'               group-name        ''
#zstyle ':completion:*' completer _oldlist _expand _force_rehash _complete
#zstyle ':completion:*'               completer         _expand      _force_rehash _complete _approximate _ignored
zstyle ':completion:*'               rehash            true
# generate descriptions with magic.
zstyle ':completion:*'               auto-description  'specify: %d'
# don't prompt for a huge list, page it!
zstyle ':completion:*:default'       list-prompt       '%s%m matches%s'
# don't prompt for a huge list, menu it!
zstyle ':completion:*:default'       menu              'select=0'
# have the newer files last so i see them first
zstyle ':completion:*'               file-sort         modification reverse
# color code completion!!!!  wohoo!
zstyle ':completion:*'               list-colors       "=(#b) #([0-9]#)*=36=31"
# separate man page sections. neat.
zstyle ':completion:*:manuals'       separate-sections true
# choose something pretty
zstyle ':completion:*'               list-separator    ' → '
# complete with a menu for xwindow ids
zstyle ':completion:*:windows'       menu              on=0
zstyle ':completion:*:expand:*'      tag-order         all-expansions
# more errors allowed for large words and fewer for small words
zstyle ':completion:*:approximate:*' max-errors        'reply=(  $((  ($#prefix+$#suffix)/3  ))  )'
# errors format
zstyle ':completion:*:corrections'   format            '%b%d (errors %e)%b'
# don't complete stuff already on the line
zstyle ':completion::*:(rm|v):*'     ignore-line       true
# don't complete directory we are already in (../here)
zstyle ':completion:*'               ignore-parents    parent       pwd
zstyle ':completion::approximate*:*' prefix-needed     false
# https://github.com/robbyrussell/oh-my-zsh/blob/master/plugins/ssh-agent/ssh-agent.plugin.zsh
zstyle ':omz:plugins:ssh-agent'      agent-forwarding  on

set completion-ignore-case on
set show-all-if-ambiguous on
# }}}


# terminal - terminfo, base16 colors, iterm integration {{{
local dot_ti=~/.terminfo/"$TERM".ti
[[ -f "$dot_ti" ]] \
    || infocmp "$TERM" \
    | sed 's/kbs=^[hh]/kbs=\\177/' \
    > "$dot_ti"
tic "$dot_ti"

if [[ "$OSX" == "$TRUE" ]]; then
    # base16-shell
    export BASE16_SHELL=~/.config/base16-shell
    if ((${#iterm_dotapp[@]})); then
        #[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"
        # make `neovim` and `tmux` play nice
        #   https://iterm2.com/documentation-shell-integration.html
        # tmux.plugin settings
        #   https://github.com/robbyrussell/oh-my-zsh/blob/master/plugins/tmux/tmux.plugin.zsh
        export ZSH_TMUX_FIXTERM=true
        #export ZSH_TMUX_ITERM2="$(((${#iterm_dotapp[@]})) && echo true || echo false)"
        #export ZSH_TMUX_FIXTERM_WITH_256COLOR="screen-256color"
        iterm2_integration=~/.iterm2_shell_integration."$SHELL_NAME"
        [[ -f "$iterm2_integration" ]] && source "$iterm2_integration"
    fi
fi
# }}}


# depends on `coreutils` {{{
export DOTFILES="`dirname "$(grealpath "$ZSHRC")"`"
export   DOTVIM="$DOTFILES/.vim"
[[ ! -f ~/.LS_COLORS ]] && \
    gdircolors ~/.dircolors > ~/.LS_COLORS
source ~/.LS_COLORS
export CLICOLOR=true
export LSCOLORS=gxbxhxdxfxhxhxhxhxcxcx
# }}}


# plugins {{{
[ -d "$DOTFILES/.zsh/complete" ] \
    && export GENCOMPL_FPATH="$DOTFILES/.zsh/complete"

autoload -U compaudit compinit
compinit -i -d ~/.zcompdump."$USER"."$SHELL"

export ANTIGEN_HS_SANDBOX='cabal'
source ~/.zsh/antigen-hs/init.zsh

command_exists z   || [ -f "$BREW"/etc/profile.d/z.sh ] \
    && source "$BREW"/etc/profile.d/z.sh
command_exists fzf || [ -f "$DOTFILES/.fzf.zsh" ] \
    && source ~/.fzf.zsh

    # zsh-users/zsh-autosuggestions {{{
        bindkey -M viins '^ '   autosuggest-accept
        bindkey -M vicmd '^ '   autosuggest-accept
        bindkey -M viins '^[[c' autosuggest-execute
        # edit-command-line
        function _autosuggest-accept-and-edit-command-line {
            zle autosuggest-accept
            zle edit-command-line
        }
        zle -N autosuggest-accept-and-edit-command-line _autosuggest-accept-and-edit-command-line
        autoload -Uz edit-command-line
        zle -N edit-command-line
        bindkey '^[[v' edit-command-line
        bindkey '^[[v' autosuggest-accept-and-edit-command-line
    # }}}
# }}}


# grc - generic colorizer {{{
source $BREW/etc/grc.bashrc # github.com/garabik/grc
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

# <M-Direction>
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
bindkey           '^[[2J'            kill-word
bindkey -M vicmd '^[[2J' vi-forward-kill-word
# <M-Backspace>
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
#bindkey -M viins ' '   magic-space

function _last-cmd-and-vi-cmd-mode {
    zle vi-cmd-mode
    zle history-substring-search-up
}
zle -N last-cmd-and-vi-cmd-mode _last-cmd-and-vi-cmd-mode
# }}}


# zsh-syntax-highlighting {{{
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


# grep {{{
export GREP_PRG='grep'
export GREP_ARGS='--color=auto'
GREP_ARGS+=' --exclude=\*.{o,pyc,.min.js}'
GREP_ARGS+=' --exclude-dir={.bzr,cvs,.git,.hg,.svn,node_modules}'

export RG_PRG='rg'
export RG_ARGS='--follow --hidden --pretty --ignore-case'
RG_ARGS+=" --ignore-file=${HOME}/.rgignore"

export AG_PRG='ag'
export AG_ARGS='--follow --hidden --nogroup'
AG_ARGS+=" --pager ${PAGER}"
AG_ARGS+=' --path-to-ignore=~/.aginore'

if   command_exists rg; then
    export GREPPRG_PRG="$RG_PRG"
    export GREPPRG_ARGS="$RG_ARGS"
    export FZF_DEFAULT_COMMAND="${RG_PRG} --vimgrep"
elif command_exists ag; then
    export GREPPRG_PRG="$AG_PRG"
    export GREPPRG_ARGS="$AG_ARGS"
    export FZF_DEFAULT_COMMAND="${AG_PRG} ${AG_ARGS} --nopager -g """
else
    export GREPPRG_PRG="$GREP_PRG"
    export GREPPRG_ARGS="$GREP_ARGS"
fi

if [ -n "${GREPPRG_PRG+x}" ] && [ -n"${GREPPRG_ARGS+x}" ]; then
    export GREPPRG="command ${GREPPRG_PRG} ${GREPPRG_ARGS}"
else
    export GREPPRG="${GREPPRG:-command grep}"
fi
# }}}


# pyenv, virtualenv {{{
export WORKON_HOME=~/.virtualenvs

# brew install pyenv \
#   pyenv-ccache pyenv-default-packages pyenv-virtualenv pyenv-virtualenvwrapper pyenv-which-ext
if command_exists pyenv; then
    #eval  "$(pyenv            init -)"
    #eval "$(pyenv virtualenv-init -)"
    export POWERLINE_CONFIG_COMMAND="`which powerline-config`"
fi

if command_exists pipenv; then
    #eval "$(env _PIPENV_COMPLETE=source-zsh pipenv)"
fi

if command_exists pew; then
    #source "$(pew shell_config)"
fi
# }}}


# nvm {{{
#source /usr/local/opt/nvm/nvm.sh
if command_exists nvm; then
    autoload -U add-zsh-hook
    function load-nvmrc() {
        local node_version="$(nvm version)"
        local nvmrc_path="$(nvm_find_nvmrc)"
        if [ -n "$nvmrc_path" ]; then
            local nvmrc_node_version=$(nvm version "$(cat "${nvmrc_path}")")
            if [ "$nvmrc_node_version" = "N/A" ]; then
                nvm install
            elif [ "$nvmrc_node_version" != "$node_version" ]; then
                nvm use
            fi
        elif [ "$node_version" != "$(nvm version default)" ]; then
            echo "Reverting to nvm default version"
            nvm use default
        fi
    }
    add-zsh-hook chpwd load-nvmrc
    load-nvmrc
fi
# }}}


# rust {{{
export RUSTUP_HOME=~/.multirust
export RUST_SRC_PATH="$RUSTUP_HOME"/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src
export CARGO_INCREMENTAL=1
export CARGO_BUILD_JOBS=$((${CORES:-4} - 1))

export APIARY_API_KEY='8ade0ff8cf5f8415c0043f91afc9cede'
# }}}


# postgres {{{
export     PGDATA="$BREW/var/postgres"
export     PGHOST="localhost"
export PGHOSTADDR="127.0.0.1"
export     PGPORT="5432"
# }}}


# vim {{{
export EDITOR='nvim'
export VISUAL="$EDITOR"
#}}}


# pager {{{
if command_exists vimpager; then
    export VIMPAGER_VIM="`which nvim`"
    export VIMPAGER_RC=~/.vimpagerrc
    export PAGER='vimpager'
else
    export PAGER='less -rfx'
fi
# }}}


# aliases {{{
. "$DOTFILES"/.aliases
aliasof() {
    local args="$([[ "$OSX" == "$TRUE" ]] \
        && printf '-e' \
        || printf '-r')"
    alias "$1" |\
        sed "$args" 's|'"$1"'=(.*)|\1|' |\
        sed "$args" "s|'||g"
}
# }}}


# history {{{
[[ -z "$HISTFILE" ]] && \
    export HISTFILE=~/.zsh_history

export HISTSIZE=$(( 128**2 ))
export SAVEHIST=$(( 128**3 ))

setopt APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS
setopt HIST_SAVE_NO_DUPS
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_VERIFY
# }}}


# note - run me last
# zsh options {{{
setopt MULTIOS
setopt CDABLEVARS

setopt NOCOMPLETE_ALIASES
setopt COMPLETE_IN_WORD

setopt PROMPT_SUBST
setopt TRANSIENT_RPROMPT

setopt IGNORE_EOF

setopt GLOB_COMPLETE
setopt EXTENDED_GLOB
setopt NO_CASE_GLOB
setopt NUMERIC_GLOB_SORT

setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
setopt PUSHDMINUS
setopt PUSHD_TO_HOME

setopt RC_EXPAND_PARAM

setopt RM_STAR_WAIT
# }}}
