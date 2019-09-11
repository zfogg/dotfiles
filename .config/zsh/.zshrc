#!/usr/bin/env zsh
# vim: set fdm=marker:
# ~/.zshrc


# zsh modules {{{
zmodload zsh/zpty

autoload -Uz          \
    select-word-style \
    zsh-mime-setup    \
    colors
    #predict-on
select-word-style normal
zsh-mime-setup
colors
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
    "$path[@]")
fi

fpath_rm=('/usr/local/share/zsh/site-functions')

fpath=(
  "${HOME}/.zsh/"{site-functions,complete}
  "${BREW}/share/zsh/"{site-functions,functions}
  ${fpath[@]/$fpath_rm})

manpath=(
  /usr/share/man
  "$manpath[@]")

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
[ -f ~/.LS_COLORS ] && source ~/.LS_COLORS
if [[ "$OSX" == "$TRUE" ]]; then
  export CLICOLOR=true
  #export LSCOLORS=gxbxhxdxfxhxhxhxhxcxcx
  export LSCOLORS=GxFxCxDxBxegedabagaced
fi
# }}}


# autocomplete {{{

set completion-ignore-case on
set show-all-if-ambiguous on

export GENCOMPL_FPATH="${HOME}/.zsh/complete"
if [[ -d "$GENCOMPL_FPATH" ]]; then
  source ~/.zsh/zsh-completion-generator/zsh-completion-generator.plugin.zsh
else
  unset GENCOMPL_FPATH
fi

zstyle ':completion:*'           special-dirs  true
zstyle ':completion:*'           accept-exact  '*(N)'
zstyle ':completion:*'           accept-exact-dirs  true
zstyle ':completion:*'           path-completion true
zstyle ':completion:*'           use-cache on
zstyle ':completion:*'           show-completer true
zstyle ':completion:*'           cache-path "${ZDOTDIR:-$HOME}/.cache"
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path "${ZDOTDIR:-$HOME}/.zcompcache"
zstyle ':predict'                verbose        true
zstyle ':completion:*'           verbose        yes

#zstyle ':completion:::::'        completer          _complete _approximate # enable approximate matches for completion

# case insensitive completion

zstyle ':completion:*'               matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
#zstyle ':completion:*'               matcher-list       'm:{a-z}={a-z}'
zstyle ':completion:*'               verbose            yes
zstyle ':completion:*:*:*:*:*'       group-name         ''
zstyle ':completion:*'               completer          _complete _match _prefix:-complete _list _correct _approximate _prefix:-approximate _ignored
#zstyle ':completion:*'               completer          _expand _complete _approximate _list _match _files _prefix _ignored
#zstyle ':completion:*'               completer          _oldlist _expand _force_rehash _complete
#zstyle ':completion:*'               completer          _oldlist _expand _force_rehash _complete _list _match _approximate
#zstyle ':completion:*'               completer          _expand _complete _correct _approximate
zstyle ':completion:*'               rehash             true
zstyle ':completion:*:match:*'       original           only

zstyle ':completion:*:approximate:*' max-errors        'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'

zstyle ':completion:*:prefix-complete:*'    completer _complete
zstyle ':completion:*:prefix-approximate:*' completer _approximate

# generate descriptions with magic.
zstyle ':completion:*'               auto-description  'specify: %d'
zstyle ':completion:*'               list-separator    ' → '
zstyle ':completion:*:default'       list-prompt       '%s%m matches%s'
zstyle ':completion:*:*:*:*:*'       menu              'select'
zstyle ':completion:*'               file-sort         modification
zstyle ':completion:*'               ignore-parents    parent pwd ..
zstyle ':completion:*'               squeeze-slashes   true

zstyle ':completion::*:(ls|cd):*'    list-dirs-first   true
zstyle ':completion::*:(ls|cd):*'    list-grouped      true

zstyle ':completion:*:manuals'       separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true

#zstyle ':completion:*' file-list list=10 insert=10
#zstyle -e ':completion:*' file-list '(( ${+NUMERIC} )) && reply=(true)'

# complete with a menu for xwindow ids
zstyle ':completion:*:windows'       menu              on=0
zstyle ':completion:*:expand:*'      tag-order         all-expansions
# more errors allowed for large words and fewer for small words

# don't complete stuff already on the line
zstyle ':completion::*:(rm|kill|diff):*' ignore-line       true
zstyle ':completion::approximate*:*'     prefix-needed     false
zstyle ':omz:plugins:ssh-agent'          agent-forwarding  on

zstyle ':completion:*:*:cd:*'                 tag-order \
  'local-directories directory-stack path-directories'
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select

zstyle ':completion:*'              group-order \
  builtins expansions aliases functions commands globbed-files \
  directories hidden-files hidden-directories \
  boring-files boring-directories keywords viewable

zstyle ':completion:*:-tilde-:*'              group-order \
  named-directories path-directories users expand

zstyle ':completion::*:(-command-|export):*' fake-parameters \
  ${${${_comps[(I)-value-*]#*,}%%,*}:#-*-}

zstyle ':completion:*:functions'    ignored-patterns '(_*|pre(cmd|exec))'
zstyle ':completion:*:*:zcompile:*' ignored-patterns '(*~|*.zwc|*.zwc.old)'
zstyle ':completion:*:sudo:*'       command-path \
  /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin
zstyle ':completion::complete:*'    gain-privileges 1


zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

zstyle ':completion:*:*:*:*:processes' command 'ps -u $USER -o pid,user,comm -w'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;36=0=01'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' insert-ids single

# formats
zstyle ':completion:*:*:*:*:*'      menu             yes select
zstyle ':completion:*:matches'      group            'yes'
zstyle ':completion:*:options'      description      'yes'
zstyle ':completion:*:options'      auto-description '%d'
zstyle ':completion:*:corrections'  format           ' %F{green}-- %d (errors: %e) --%f'
zstyle ':completion:*:descriptions' format           ' %F{yellow}-- %d --%f'
zstyle ':completion:*:messages'     format           ' %F{purple} -- %d --%f'
zstyle ':completion:*:warnings'     format           ' %F{red}-- no matches found --%f'
zstyle ':completion:*'              format           ' %F{yellow}-- %d --%f'
zstyle ':completion:*:default'      list-prompt      '%S%M matches%s'
zstyle ':completion:*:default'      select-prompt    '%p%s'

# Array completion element sorting.
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

zstyle ':completion:*:*:-command-:*:*' file-patterns \
 '*(#q-*):executables:Executables *(-/):directories:Directories'

zstyle -e ':completion:*:*:-command-:*:*' tag-order '
  reply=(
          "executables:Executables:Executables
          builtins:Builtins:Builtins
          commands:Commands:Commands
          aliases:Aliases:Aliases
          functions:Functions:Functions
          parameters:Variables:Variables
          reserved-words:Keywords:Keywords
          Directories:directories"
          "-"
        )
'

zstyle ':completion:*:history-words' stop yes
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' list false
zstyle ':completion:*:history-words' menu yes

zstyle :compinstall filename "${ZDOTDIR:-$HOME}/.zshrc"
# }}}


# antigen-hs {{{
zmodload zsh/complist
autoload -Uz compinit
for dump in "${ZDOTDIR:-$HOME}"/.zcompdump(N.mh+24); do
  compinit
done
compinit -C

export ANTIGEN_HS_SANDBOX='stack'
source ~/.zsh/antigen-hs/init.zsh

export CORES="`nproc`"
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

  # grc - generic colorizer {{{
    # INFO: github.com/garabik/grc
    [[ -f "$iterm2_integration" ]] && source "${BREW}/etc/grc.bashrc"
  # }}} grc - generic colorizer
fi
# }}}


# plugins {{{
command_exists z   || [ -f "${BREW}/etc/profile.d/z.sh" ] \
    && source "${BREW}/etc/profile.d/z.sh"
command_exists fzf \
  && [ -f "/usr/share/fzf/key-bindings.zsh" ] \
  && [ -f "/usr/share/fzf/completion.zsh" ] \
    && source /usr/share/fzf/{key-bindings,completion}.zsh

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
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic
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
export ZSH_PYENV_LAZY_VIRTUALENV=true
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
#if command_exists direnv; then
  #eval "$(direnv hook zsh)"
#fi
# }}}


# rust {{{
export RUSTUP_HOME=~/.rustup
if [[ "${OSX:-0}" == "${TRUE:-1}" ]]; then
  export RUST_SRC_PATH="${RUSTUP_HOME:-~/.rustup}/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"
elif [[ "${LINUX:-0}" == "${TRUE:-1}" ]]; then
  export RUST_SRC_PATH="${RUSTUP_HOME:-~/.rustup}/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"
fi
export CARGO_INCREMENTAL=1
export CARGO_BUILD_JOBS="$((${CORES:-2} - 1))"
# }}}


# postgres {{{
if [[ "$OSX" == "$TRUE" ]]; then
  export     PGDATA="$BREW/var/postgres"
  export     PGHOST="localhost"
  export PGHOSTADDR="127.0.0.1"
  export     PGPORT="5432"
fi
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

setopt   ALWAYS_TO_END
setopt   APPEND_HISTORY
setopt   AUTO_LIST
setopt   AUTO_MENU
setopt   AUTO_PARAM_SLASH
setopt   AUTO_PUSHD
setopt   BANG_HIST
unsetopt BEEP
unsetopt CASE_GLOB
setopt   CDABLEVARS
unsetopt COMPLETE_ALIASES
setopt   COMPLETE_IN_WORD
setopt   EXTENDED_GLOB
setopt   EXTENDED_HISTORY
unsetopt FLOW_CONTROL
setopt   GLOB_COMPLETE
setopt   GLOB_DOTS
setopt   HIST_EXPIRE_DUPS_FIRST
setopt   HIST_IGNORE_ALL_DUPS
setopt   HIST_IGNORE_DUPS
setopt   HIST_IGNORE_SPACE
setopt   HIST_REDUCE_BLANKS
unsetopt HIST_SAVE_NO_DUPS
setopt   HIST_VERIFY
setopt   IGNORE_EOF
setopt   INC_APPEND_HISTORY
setopt   INTERACTIVE_COMMENTS
unsetopt LIST_AMBIGUOUS
unsetopt MENU_COMPLETE
setopt   MULTIOS
unsetopt NOMATCH
unsetopt NOTIFY
setopt   NUMERIC_GLOB_SORT
setopt   PATH_DIRS
setopt   PROMPT_SUBST
setopt   PUSHDMINUS
setopt   PUSHD_IGNORE_DUPS
setopt   PUSHD_TO_HOME
setopt   RC_EXPAND_PARAM
setopt   RM_STAR_WAIT
setopt   SHARE_HISTORY
setopt   TRANSIENT_RPROMPT

# }}}


# aliases {{{
source ~/.aliases
function aliasof() {
  local args="$([[ "$OSX" == "$TRUE" ]] \
    && printf '-e' \
    || printf '-r')"
  alias "$1" |\
    command -p sed "$args" 's|'"$1"'=(.*)|\1|' |\
    command -p sed "$args" "s|'||g"
}


function() {
  autoload -Uz add-zsh-hook
  zshcache_time="$(date +%s%N)"
  function rehash_precmd() {
    if [[ -a /var/cache/zsh/pacman ]]; then
      local paccache_time="$(date -r /var/cache/zsh/pacman +%s%N)"
      if (( zshcache_time < paccache_time )); then
        rehash
        zshcache_time="$paccache_time"
      fi
    fi
  }
  add-zsh-hook -Uz precmd rehash_precmd
}
# }}}


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
