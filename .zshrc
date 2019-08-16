#!/usr/bin/env zsh
# vim: set fdm=marker:
# ~/.zshrc


# note: meta helpers {{{
command_exists() { command -v "$1" 2>/dev/null 1>&2 }
alias_exists()   { alias      "$1" 2>/dev/null 1>&2 }
# }}}


# zsh modules {{{
zmodload zsh/zpty

autoload -Uz          \
    url-quote-magic   \
    select-word-style \
    colors            \
    zsh-mime-setup

select-word-style normal
colors
zsh-mime-setup
# }}}


# autocomplete {{{
#source ~/.zsh/zsh-completion-generator/zsh-completion-generator.plugin.zsh

_force_rehash() {
  (( CURRENT == 1 )) && rehash
  return 1  # Because we didn't really complete anything
}

zstyle ':completion:*'               special-dirs      true
zstyle ':completion::complete:*'     use-cache         1

# case insensitive completion
zstyle ':completion:*'               matcher-list       'm:{a-z}={a-z}'
zstyle ':completion:*'               verbose            yes
zstyle ':completion:*:descriptions'  format             '%b%d%b'
zstyle ':completion:*:messages'      format             '%d'
zstyle ':completion:*:warnings'      format             'no matches for: %d'
zstyle ':completion:*'               group-name         ''
zstyle ':completion:*'               completer _oldlist _expand _force_rehash _complete
zstyle ':completion:*'               completer          _expand      _force_rehash _complete _approximate _ignored
zstyle ':completion:*'               rehash             true

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
# NOTE: https://github.com/robbyrussell/oh-my-zsh/blob/master/plugins/ssh-agent/ssh-agent.plugin.zsh
zstyle ':omz:plugins:ssh-agent'      agent-forwarding  on
export DISABLE_MAGIC_FUNCTIONS=true # github.com/robbyrussell/oh-my-zsh/issues/5569
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
    if [[ "$ITERM_DOTAPP" == "true" || "$ITERM_DOTAPP" == "$TRUE" ]]; then
        #[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"
        # make `neovim` and `tmux` play nice
        #   https://iterm2.com/documentation-shell-integration.html
        # tmux.plugin settings
        #   https://github.com/robbyrussell/oh-my-zsh/blob/master/plugins/tmux/tmux.plugin.zsh
        export ZSH_TMUX_FIXTERM=true
        #export ZSH_TMUX_ITERM2="$(((${#iterm_dotapp[@]})) && echo true || echo false)"
        #export ZSH_TMUX_FIXTERM_WITH_256COLOR="screen-256color"
        iterm2_integration=~/.iterm2_shell_integration."${SHELL:t}"
        [[ -f "$iterm2_integration" ]] && source "$iterm2_integration"
    fi
fi
# }}}


# depends on `coreutils` {{{
if [[ "$OSX" == "$TRUE" ]]; then
    export ZSHRC_PATH=`greadlink -f "${(%):-%N}"`
    export DOTFILES=`gdirname -- "$ZSHRC_PATH"`
    [[ ! -f ~/.LS_COLORS ]] && \
        gdircolors ~/.dircolors > ~/.LS_COLORS
else
    export ZSHRC_PATH=`readlink -f "${(%):-%N}"`
    export DOTFILES=`dirname -- "$ZSHRC_PATH"`
    [[ ! -f ~/.LS_COLORS ]] && \
        dircolors ~/.dircolors > ~/.LS_COLORS
fi
export  DOTVIM="${DOTFILES}/.vim"
source ~/.LS_COLORS
export CLICOLOR=true
export LSCOLORS=gxbxhxdxfxhxhxhxhxcxcx
# }}}


# nvm {{{
export NVM_DIR="${HOME}/.nvm"
export  NVM_LAZY_LOAD='true'
[[ ! -v NVM_LAZY_LOAD ]] \
  && echo sourcing nvm && source /usr/local/opt/nvm/nvm.sh

#if command_exists nvm; then # {{{
    #autoload -U add-zsh-hook
    #function load-nvmrc() {
        #local nodever="$(nvm version)"
        #local nvmrc="$(nvm_find_nvmrc)"
        #if [ -n "$nvmrc" ]; then
            #local nvmver="$(nvm version "`cat ${nvmrc}`")"

            #if [ "$nvmver" = "N/A" ]; then
                #nvm install

            #elif [ "$nvmver" != "$nodever" ];
                #nvm use

        #elif [ "$nodever" != "$(nvm version default)" ]; then
            #echo "Reverting to nvm default version"
            #nvm use default
        #fi
    #} # /function load-nvmrc()

    #add-zsh-hook chpwd load-nvmrc
    #load-nvmrc
#fi # }}}
# }}}


# plugins {{{
#export ZCOMPDUMP="~/.zsh/zcompdump.${USER}.${SHELL:t}"
#compinit -d "$ZCOMPDUMP"
#
autoload -U compinit && compinit -d

export ANTIGEN_HS_SANDBOX='cabal'
source ~/.zsh/antigen-hs/init.zsh

command_exists z   || [ -f "${BREW}/etc/profile.d/z.sh" ] \
    && source "${BREW}/etc/profile.d/z.sh"
command_exists fzf || [ -f "${DOTFILES}/.fzf.zsh" ] \
    && source ~/.fzf.zsh

#   zsh-users/zsh-autosuggestions {{{
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
#   }}}

# }}} plugins


# grc - generic colorizer {{{
# github.com/garabik/grc
[[ -f "$iterm2_integration" ]] && source $BREW/etc/grc.bashrc
# }}} grc - generic colorizer


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
bindkey          '^[[2J'            kill-word
#bindkey -M vicmd '^[[2J' vi-forward-kill-word
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
    # patterns
    ZSH_HIGHLIGHT_PATTERNS+=('rm -*' fg=grey,bold,underline,bg=red)
    ZSH_HIGHLIGHT_PATTERNS+=('sudo*' fg=white,bold,bg=red)
    # styles
    ZSH_HIGHLIGHT_STYLES[default]=none
    ZSH_HIGHLIGHT_STYLES[unknown-token]=fg=248
    ZSH_HIGHLIGHT_STYLES[reserved-word]=fg=009,standout
    ZSH_HIGHLIGHT_STYLES[alias]=fg=cyan,bold
    ZSH_HIGHLIGHT_STYLES[builtin]=fg=145,bold,underline
    ZSH_HIGHLIGHT_STYLES[function]=fg=cyan,bold
    ZSH_HIGHLIGHT_STYLES[command]=fg=white,bold
    ZSH_HIGHLIGHT_STYLES[precommand]=fg=white,underline
    #ZSH_HIGHLIGHT_STYLES[commandseparator]=none
    ZSH_HIGHLIGHT_STYLES[hashed-command]=fg=009,standout
    ZSH_HIGHLIGHT_STYLES[path]=fg=204,underline
    ZSH_HIGHLIGHT_STYLES[globbing]=fg=063
    ZSH_HIGHLIGHT_STYLES[history-expansion]=fg=underline
    ZSH_HIGHLIGHT_STYLES[single-hyphen-option]=fg=137,bold
    ZSH_HIGHLIGHT_STYLES[double-hyphen-option]=fg=244,italic
    ZSH_HIGHLIGHT_STYLES[back-quoted-argument]=fg=244,underline
    ZSH_HIGHLIGHT_STYLES[single-quoted-argument]=fg=137,bold
    ZSH_HIGHLIGHT_STYLES[double-quoted-argument]=fg=244
    ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]=fg=148,bold
    ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]=fg=148,bold
    ZSH_HIGHLIGHT_STYLES[assign]=fg=246
fi
# }}}


# grep {{{
function() {
  if command_exists rg; then
    local RG_PRG='rg'
    local RG_ARGS='--follow --hidden --pretty --ignore-case'
    RG_ARGS+=" --ignore-file=${HOME}/.rgignore"
    RG_ARGS+=" --before-context 1"
    RG_ARGS+=" --after-context  1"
    RG_ARGS+=" --max-columns 512"
    local GREPPRG_PRG="$RG_PRG"
    local GREPPRG_ARGS="$RG_ARGS"
    export FZF_DEFAULT_COMMAND="${RG_PRG} --vimgrep "

  elif command_exists ag; then
    local AG_PRG='ag'
    local AG_ARGS='--follow --hidden --nogroup'
    AG_ARGS+=" --pager ${PAGER}"
    AG_ARGS+=' --path-to-ignore=~/.aginore'
    local GREPPRG_PRG="$AG_PRG"
    local GREPPRG_ARGS="$AG_ARGS"
    export FZF_DEFAULT_COMMAND="${AG_PRG} ${AG_ARGS} --nopager -g "

  else # INFO: "command_exists grep; then"
    local GREP_PRG='grep'
    local GREP_ARGS='--color=auto'
    GREP_ARGS+=' --exclude=\*.{o,pyc,.min.js}'
    GREP_ARGS+=' --exclude-dir={.bzr,cvs,.git,.hg,.svn,node_modules}'
    local GREPPRG_PRG="$GREP_PRG"
    local GREPPRG_ARGS="$GREP_ARGS"
    export FZF_DEFAULT_COMMAND="${GREP_PRG} ${GREP_ARGS} -g "
  fi

  [[ -v GREPPRG_PRG \
  && -v GREPPRG_ARGS ]] \
      && export GREPPRG="command ${GREPPRG_PRG} ${GREPPRG_ARGS}"\
      || export GREPPRG="${GREPPRG:-command -p grep}"
}
# }}}


# Vagrant {{{
#export VAGRANT_DEFAULT_PROVIDER='parallels'
# }}}


# pyenv, virtualenv {{{
#export WORKON_HOME="${HOME}/.virtualenvs"

# brew install pyenv \
#   pyenv-ccache pyenv-default-packages pyenv-virtualenv pyenv-virtualenvwrapper pyenv-which-ext
#if command_exists pyenv; then
#    #eval "$(pyenv            init -)"
#    #eval "$(pyenv virtualenv-init -)"
#    export POWERLINE_CONFIG_COMMAND="`which powerline-config`"
#fi

#if command_exists pipenv; then
    #eval "$(env _PIPENV_COMPLETE=source-zsh pipenv)"
#fi

#if command_exists pew; then
    #source "$(pew shell_config)"
#fi
# }}}

# ruby, rbenv, rvm {{{
#eval "$(rbenv init -)"
# ruby, rbenv, rvm }}}

# jenv {{{
#if command_exists jenv; then
  #eval "$(jenv init -)"
#fi
# }}}

# direnv {{{
#if command_exists direnv; then
  #eval "$(direnv hook zsh)"
#fi
# }}}

# rust {{{
export RUSTUP_HOME="${HOME}/.multirust"
export RUST_SRC_PATH="${RUSTUP_HOME}/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"
export RUSTFLAGS='-C target-cpu=native'
export CARGO_INCREMENTAL='1'
export CARGO_BUILD_JOBS="$((${CORES:-4} - 1))"

# }}}


# postgres {{{
export     PGDATA="${BREW}/var/postgres"
export     PGHOST='localhost'
export PGHOSTADDR='127.0.0.1'
export     PGPORT='5432'
# }}}


# vim {{{
export EDITOR='nvim'
export VISUAL="${EDITOR}"
export MANPAGER="${EDITOR} -c 'set ft=man' -"
export LESS='-R'
export LESSOPEN='| vimcat -o - %s'
#}}}


# pager {{{
if command_exists vimpager; then
    export VIMPAGER_VIM="`which vim`"
    export VIMPAGER_RC="${HOME}/.vimpagerrc"
    export PAGER='vimpager'
else
    export PAGER='less -rfx'
fi
# }}}


# aliases {{{
source "${DOTFILES}/.aliases"
function aliasof() {
    local args="$([[ "$OSX" == "$TRUE" ]] \
        && printf '-E' \
        || printf '-r')"
    alias "$1" |\
        command -p sed "$args" 's|'"$1"'=(.*)|\1|' |\
        command -p sed "$args" "s|'||g"
}

unset MAILCHECK
# }}}


# history {{{
[[ -z "$HISTFILE" ]] && \
    export HISTFILE=~/.zsh_history

export HISTSIZE=16384   # == 128**2
export SAVEHIST=65536   # == 256**2

set +o histexpand
setopt   APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_SAVE_NO_DUPS
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_VERIFY
setopt BANG_HIST

# unset
unsetopt HIST_REDUCE_BLANKS
# }}}


# zsh options {{{ NOTE: run me last
setopt   MULTIOS
setopt   CDABLEVARS

set      completion-ignore-case on
setopt   NOCOMPLETE_ALIASES
setopt   COMPLETE_IN_WORD
unsetopt MENU_COMPLETE
setopt   COMPLETE_IN_WORD

setopt   PROMPT_SUBST
setopt   TRANSIENT_RPROMPT

setopt   IGNORE_EOF

setopt   GLOB_COMPLETE
setopt   EXTENDEDGLOB
setopt   NO_CASE_GLOB
setopt   NUMERIC_GLOB_SORT

setopt   AUTO_PUSHD
setopt   PUSHD_IGNORE_DUPS
setopt   PUSHDMINUS
setopt   PUSHD_TO_HOME

setopt   RC_EXPAND_PARAM

setopt   RM_STAR_WAIT

set      show-all-if-ambiguous on
unsetopt LIST_AMBIGUOUS

setopt   INTERACTIVE_COMMENTS
# }}}
