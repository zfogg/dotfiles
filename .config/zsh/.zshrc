#!/usr/bin/env zsh
# vim: set fdm=marker:
# ~/.zshrc



# zsh modules {{{
zmodload zsh/zpty

#autoload -Uz          \
      #colors            \
#colors

autoload -Uz          \
    url-quote-magic   \
    select-word-style \
    zsh-mime-setup
    #predict-on
select-word-style normal
zsh-mime-setup
#predict-on
# }}}


# path, manpath, fpath {{{
typeset -U path
typeset -U fpath
typeset -U manpath

typeset -aU classpath
typeset -xT CLASSPATH classpath
typeset -aU infopath
typeset -xT INFOPATH infopath


if [[ "$OSX" == "$TRUE" ]]; then
  path=(
    ~/bin
    #~/.config/composer/vendor/bin
    #~/.platformio/penv/bin
    #"$BREW"/opt/php71/bin
    "$BREW"/opt/node/bin
    "$BREW"/opt/ruby/bin
    "$PYENV_ROOT"/shims
    #~/.jenv/shims
    ~/.local/bin
    ~/.{cargo,gem}/bin
    #~/.cabal/bin
    "$GOPATH"/bin
    "$GOROOT"/bin
    "$BREW"/sbin
    "$BREW"/MacGPG2/bin
    "$BREW"/bin
    $path
    #$(/usr/bin/getconf PATH | /usr/bin/tr ':' '\n' | /usr/bin/tail -r)
  )

else
  path=(
    ~/bin
    ~/.local/bin
    ~/.{cabal,cargo,gem}/bin
    "$PYENV_ROOT"/shims
    "$GOPATH"/bin
    "$GOROOT"/bin
    $path
  )
    #$(echo $PATH | tr ':' '\n' | tac)
    #$(getconf PATH | tr ':' '\n' | tac))
fi

fpath=(
  ~/.zsh/site-functions
  ~/.zsh/complete
  #$BREW/share/zsh-completions
  $BREW/share/zsh/{site-functions,functions}
  $fpath
)

manpath=(
  /usr/share/man
  $manpath
)

if [[ "$OSX" == "$TRUE" ]]; then
  local brew_gnu_progs=(
  coreutils
  findutils
  #gnu-sed
  #gnu-which
  gnu-tar)
  path=(
    #"${BREW}/opt/${^brew_gnu_progs}/libexec/gnubin"
    "${BREW}/opt/coreutils/libexec/gnubin"
    "${BREW}/opt/findutils/libexec/gnubin"
    "${BREW}/opt/gnu-tar/libexec/gnubin"
    "${BREW}/opt/ccache/libexec"
    #"$iOSOpenDevPath"/bin
    #"$THEOS"/bin
    "${BREW}/MacGPG2/bin"
    $path
  )
  #fpath=(
  #  $fpath
  #)
  manpath=(
    #${BREW}/Cellar/*/*/{share/man,libexec/gnuman}
    ${BREW}/opt/${^brew_gnu_progs}"/libexec/gnuman"
    ${BREW}/share/man
    $manpath
  )
    infopath=(
      ${BREW}/opt/${^brew_gnu_progs}/share/info
      $infopath
    )
fi

export PATH
export MANPATH
export FPATH
export CLASSPATH
export INFOPATH
# path, manpath, fpath }}}


# depends on `coreutils` {{{
if [[ "$OSX" == "$TRUE" ]]; then
  source ~/.LS_COLORS
  export CLICOLOR=true
  #export LSCOLORS=gxbxhxdxfxhxhxhxhxcxcx
  export LSCOLORS=GxFxCxDxBxegedabagaced
fi
# }}}


# autocomplete {{{
source ~/.zsh/zsh-completion-generator/zsh-completion-generator.plugin.zsh

_force_rehash() {
  (( CURRENT == 1 )) && rehash
  return 1  # Because we didn't really complete anything
}

zstyle ':completion:*'           special-dirs   true
zstyle ':completion::complete:*' use-cache      1
zstyle ':completion:*'           cache-path     "${ZDOTDIR:-~}/history"
zstyle ':completion:*'           show-completer true
zstyle :predict                  verbose        true
zstyle ':completion:*'           verbose        yes

zstyle ':completion:::::'            completer          _complete _approximate # enable approximate matches for completion

# case insensitive completion
zstyle ':completion:*'               matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
#zstyle ':completion:*'               matcher-list       'm:{a-z}={a-z}'
zstyle ':completion:*'               verbose            yes
zstyle ':completion:*:descriptions'  format             '%b%d%b'
zstyle ':completion:*:messages'      format             '%d'
zstyle ':completion:*'               group-name         ''
#zstyle ':completion:*'               completer          _complete _approximate _list _match _files _prefix _ignored
#zstyle ':completion:*'               completer          _oldlist _expand _force_rehash _complete
#zstyle ':completion:*'               completer          _oldlist _expand _force_rehash _complete _list _match _approximate
zstyle ':completion:*'               completer          _expand _complete _correct _approximate
zstyle ':completion:*'               rehash             true

zstyle ':completion:*:approximate:*' max-errors        'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'

# generate descriptions with magic.
zstyle ':completion:*'               auto-description  'specify: %d'
# don't prompt for a huge list, page it!
zstyle ':completion:*:default'       list-prompt       '%s%m matches%s'
# don't prompt for a huge list, menu it!
zstyle ':completion:*:*:*:*:*'       menu              'select'
# have the newer files last so i see them first
zstyle ':completion:*'               file-sort         modification
# color code completion!!!!  wohoo!
#zstyle ':completion:*:default'       list-colors       ${(s.:.)LS_COLORS}
# separate man page sections. neat.
zstyle ':completion:*:manuals'       separate-sections true
# choose something pretty
zstyle ':completion:*'               list-separator    ' → '

# complete with a menu for xwindow ids
zstyle ':completion:*:windows'       menu              on=0
zstyle ':completion:*:expand:*'      tag-order         all-expansions
# more errors allowed for large words and fewer for small words
# errors format
zstyle ':completion:*:corrections'   format            '%b%d (errors %e)%b'

# don't complete stuff already on the line
zstyle ':completion::*:(rm|v):*'     ignore-line       true
# don't complete directory we are already in (../here)
zstyle ':completion:*'               ignore-parents    parent       pwd
zstyle ':completion::approximate*:*' prefix-needed     false
# NOTE: https://github.com/robbyrussell/oh-my-zsh/blob/master/plugins/ssh-agent/ssh-agent.plugin.zsh
zstyle ':omz:plugins:ssh-agent'      agent-forwarding  on


zstyle ':completion:*:functions'    ignored-patterns '_*'
zstyle ':completion:*:*:zcompile:*' ignored-patterns '(*~|.*zwc)'
zstyle ':completion:*:sudo:*'       command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin


if [[ "$CLICOLOR" = 1 ]]; then
  zstyle    ':completion:*:*:kill:*:processes' list-colors "=(#b) #([0-9]#)*=36=31"
  zstyle    ':completion:*:warnings'           format $'%{\e[0;31m%}No matches for:%{\e[0m%} %d'
  zstyle -e ':completion:*:default'            list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)*==36=36}:${(s.:.)LS_COLORS}")';
  zstyle    ':completion:*'                    list-colors ${(s.:.)LS_COLORS} 'ma=7;33'
else
  zstyle ':completion:*:warnings' format $'No matches for: %d'
fi

#export DISABLE_MAGIC_FUNCTIONS=true # github.com/robbyrussell/oh-my-zsh/issues/5569

#zstyle ':completion:*:*:*:*:*'       menu               yes select

zstyle ':completion:*' squeeze-slashes true

# }}}


# antigen-hs {{{
zmodload zsh/complist
autoload -Uz compinit
for dump in "${ZDOTDIR:-~}"/.zcompdump(N.mh+24); do
  compinit
done
compinit -C

export ANTIGEN_HS_SANDBOX='stack'
source ~/.zsh/antigen-hs/init.zsh
# }}}


# terminal - terminfo, base16 colors, iterm integration {{{
#local dot_ti=~/.terminfo/"$TERM".ti
#[[ -f "$dot_ti" ]] \
#    || infocmp "$TERM" \
#    | sed 's/kbs=^[hh]/kbs=\\177/' \
#    > "$dot_ti"
#tic "$dot_ti"

if [[ "$OSX" == "$TRUE" ]]; then
  export BASE16_SHELL="${HOME}/.config/base16-shell"
  if [[ -v ITERM_SESSION_ID ]]; then
    # INFO: base16-shell - https://github.com/chriskempson/base16-shell
    #[ -n "$PS1" ] && \
      #[ -s "${BASE16_SHELL}/profile_helper.sh" ] && \
      #eval "$("${BASE16_SHELL}/profile_helper.sh")"

    # INFO: make `neovim` and `tmux` play nice
    #   https://iterm2.com/documentation-shell-integration.html
    # tmux.plugin settings
    #   https://github.com/robbyrussell/oh-my-zsh/blob/master/plugins/tmux/tmux.plugin.zsh
    #export ZSH_TMUX_FIXTERM=true
    #export ZSH_TMUX_ITERM2="$(((${#iterm_dotapp[@]})) && echo true || echo false)"
    #export ZSH_TMUX_FIXTERM_WITH_256COLOR="screen-256color"

    function() {
      local iterm2_integration=~/.iterm2_shell_integration."${SHELL_NAME:t}"
      [[ -f "$iterm2_integration" ]] && source "$iterm2_integration"
    }
  fi
fi
# }}}


# plugins {{{
command_exists z   || [ -f "${BREW}/etc/profile.d/z.sh" ] \
    && source "${BREW}/etc/profile.d/z.sh"
command_exists fzf || [ -f "${DOTFILES}/.fzf.zsh" ] \
    && source ~/.fzf.zsh

# zsh-users/zsh-autosuggestions {{{
  source ~/.zsh/zsh-autosuggestions.config.zsh
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
# zsh-users/zsh-autosuggestions }}}

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
bindkey -M viins 'hH'    beginning-of-line
bindkey -M viins 'lL'          end-of-line
bindkey -M vicmd 'hH' vi-beginning-of-line
bindkey -M vicmd 'lL'       vi-end-of-line

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
bindkey -M viins '^[[3~'    vi-delete-char
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
# FIXME

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
#zle -N self-insert url-quote-magic
bindkey -M viins ' '   magic-space

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
    ZSH_HIGHLIGHT_STYLES[commandseparator]=none
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
    export RIPGREP_CONFIG_PATH="${HOME}/.ripgreprc"
    local RG_PRG='rg'
    local GREPPRG_PRG="$RG_PRG"
    local GREPPRG_ARGS="$RG_ARGS"
    export FZF_DEFAULT_COMMAND="${RG_PRG} --vimgrep "

  else # INFO: "command_exists grep; then"
    local GREP_PRG='grep'
    local GREP_ARGS='--color=auto'
    GREP_ARGS+=' --exclude=\*.{o,pyc,.min.js}'
    GREP_ARGS+=' --exclude-dir={.bzr,cvs,.git,.hg,.svn,node_modules}'
    local GREPPRG_PRG="$GREP_PRG"
    local GREPPRG_ARGS=" $GREP_ARGS"
    export FZF_DEFAULT_COMMAND="${GREP_PRG} ${GREP_ARGS} -g "
  fi

  [[ -v GREPPRG_PRG \
  && -v GREPPRG_ARGS ]] \
      && export GREPPRG="${GREPPRG_PRG}${GREPPRG_ARGS}" \
      || export GREPPRG="${GREPPRG:-command -p grep}"
}
# }}}


# Vagrant {{{
#export VAGRANT_DEFAULT_PROVIDER='parallels'
# }}}


# pyenv, virtualenv {{{
export WORKON_HOME=~/.virtualenvs

# brew install pyenv \
#   pyenv-ccache pyenv-default-packages pyenv-virtualenv pyenv-virtualenvwrapper pyenv-which-ext
export ZSH_PYENV_LAZY_VIRTUALENV=false
if command_exists pyenv; then
   #eval "$(pyenv            init -)"
   #eval "$(pyenv virtualenv-init -)"
   #export POWERLINE_CONFIG_COMMAND="`which powerline-config`"
fi

#if command_exists pipenv; then
    #eval "$(env _PIPENV_COMPLETE=source-zsh pipenv)"
#fi

#if command_exists pew; then
    #source "$(pew shell_config)"
#fi
# }}}


# npm, nvm {{{
#source /usr/local/opt/nvm/nvm.sh
#if command_exists nvm; then
#    autoload -U add-zsh-hook
#    function load-nvmrc() {
#        local node_version="$(nvm version)"
#        local nvmrc_path="$(nvm_find_nvmrc)"
#        if [ -n "$nvmrc_path" ]; then
#            local nvmrc_node_version=$(nvm version "$(cat "${nvmrc_path}")")
#            if [ "$nvmrc_node_version" = "N/A" ]; then
#                nvm install
#            elif [ "$nvmrc_node_version" != "$node_version" ]; then
#                nvm use
#            fi
#        elif [ "$node_version" != "$(nvm version default)" ]; then
#            echo "Reverting to nvm default version"
#            nvm use default
#        fi
#    }
#    add-zsh-hook chpwd load-nvmrc
#    load-nvmrc
#fi
# }}}

# jenv {{{
#if command_exists jenv; then
  #eval "$(jenv init -)"
#fi
# }}}

# direnv {{{
export AUTOENV_DISABLED=0
export AUTOENV_FILE_ENTER=.env
export AUTOENV_HANDLE_LEAVE=0
export AUTOENV_LOOK_UPWARDS=0
if command_exists direnv; then
  eval "$(direnv hook zsh)"
fi
# }}}

# rust {{{
export RUSTUP_HOME=~/.multirust
export RUST_SRC_PATH="$RUSTUP_HOME"/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src
export CARGO_INCREMENTAL=1
export CARGO_BUILD_JOBS=$((${CORES:-4} - 1))
# }}}


# postgres {{{
export     PGDATA="$BREW/var/postgres"
export     PGHOST="localhost"
export PGHOSTADDR="127.0.0.1"
export     PGPORT="5432"
# }}}



# aliases {{{
. ~/.aliases
aliasof() {
    local args="$([[ "$OSX" == "$TRUE" ]] \
        && printf '-e' \
        || printf '-r')"
    alias "$1" |\
        command -p sed "$args" 's|'"$1"'=(.*)|\1|' |\
        command -p sed "$args" "s|'||g"
}

unset MAILCHECK
# }}}


# zsh startup debug (BOTTOM of ~/.zshrc) {{{
#   https://kev.inburke.com/kevin/profiling-zsh-startup-time
if [[ ! -z "$SHELL_DEBUG" ]]; then
  unsetopt xtrace
  exec 2>&3 3>&-
fi
# zsh startup debug (BOTTOM of ~/.zshrc) }}}


# history {{{
[[ -z "$HISTFILE" ]] && \
    export HISTFILE="${ZDOTDIR:-~}/.zsh_history"

export HISTSIZE=16384   # == 128**2
export SAVEHIST=65536   # == 256**2

set +o histexpand
setopt   APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
unsetopt HIST_SAVE_NO_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY
setopt BANG_HIST
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY

# unset
unsetopt HIST_REDUCE_BLANKS


setopt MULTIOS
setopt CDABLEVARS

set      completion-ignore-case on
setopt   COMPLETE_ALIASES
setopt   COMPLETE_IN_WORD
setopt   MENU_COMPLETE
#setopt   FLOWCONTROL
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


set show-all-if-ambiguous on
unsetopt LIST_AMBIGUOUS

setopt   INTERACTIVE_COMMENTS

unsetopt NOMATCH
unsetopt NOTIFY

unsetopt BEEP

setopt AUTOLIST
setopt ALWAYS_TO_END
# }}}
