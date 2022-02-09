#!/usr/bin/env zsh
# vim: set fdm=marker:


zmodload -i zsh/complist

# zstyle {{{
zstyle ':completion:*'           special-dirs      true
zstyle ':completion:*'           accept-exact      '*(N)'
zstyle ':completion:*'           accept-exact-dirs true
zstyle ':completion:*'           path-completion   true
zstyle ':completion:*'           show-completer    true
zstyle ':predict'                verbose           true
zstyle ':completion:*'           verbose           yes

zstyle ':completion:*'           use-cache on
zstyle ':completion:*'           cache-path "$ZDOTDIR/.zcompcache"
#zstyle ':completion::complete:*' use-cache on
#zstyle ':completion:*'           cache-path "$ZDOTDIR/.cache"
#zstyle ':completion::complete:*' cache-path "$ZDOTDIR/.zcompcache"

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

if [[ $OSX == $TRUE ]]; then
  zstyle ':completion:*'               rehash             true
elif [[ $LINUX == $TRUE ]]; then
  # INFO: https://wiki.archlinux.org/index.php/Zsh#Alternative_on-demand_rehash_using_SIGUSR1
  # requires /etc/pacman.d/hooks/zsh-rehash.hook
  trap 'rehash' USR1
else
  zstyle ':completion:*'               rehash             true
fi

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
zstyle ':completion:*'               ignore-parents    parent pwd
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

zstyle ':completion:*:cd:*' ignore-parents parent pwd
zstyle ':completion:*:cd:*' tag-order \
  local-directories directory-stack path-directories
zstyle ':completion:*:directory-stack' list-colors \
  '=(#b) #([0-9]#)*( *)==95=38;5;12'

zstyle ':completion:*'              group-order \
  builtins expansions aliases functions commands globbed-files \
  directories hidden-files hidden-directories \
  boring-files boring-directories keywords viewable

zstyle ':completion:*:-tilde-:*'              group-order \
  named-directories path-directories users expand

zstyle ':completion::*:(-command-|export):*' fake-parameters \
  ${${${_comps[(I)-value-*]#*,}%%,*}:#-*-}

zstyle ':completion:*:functions'        ignored-patterns '(_*|pre(cmd|exec))'
zstyle ':completion:*:*:zcompile:*'     ignored-patterns '(*~|*.zwc|*.zwc.old)'
zstyle ':completion::complete:ls(d|):*' ignored-patterns '(*.zwc|*.zwc.old)'
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
#zstyle ':completion:*:*:*:*:*'      menu             yes select
zstyle ':completion:*:*:*:*:*'      menu             select
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
  '*(#q-*):executables:Executables' \
  '*(-/):directories:Directories' \
  '*:all-files'

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

zstyle :compinstall filename "${ZDOTDIR:-~/.config/zsh}/z/complete.zsh"
# zstyle }}}


# NOTE: run this BEFORE sourcing 3rd-party completions
# compinit {{{
function() {
  autoload -Uz +X compinit
  autoload -Uz +X bashcompinit

  local zcompdump=$ZDOTDIR/.zcompdump
  [[ $OSX == $TRUE && -d $BREW/opt/coreutils ]] && alias date=gdate
  [[ -f $zcompdump && $(date +'%j') > $(date +'%j' -r $zcompdump) ]] \
    && compinit \
    || compinit -C

  alias date >/dev/null && unalias date

  bashcompinit
}

# INFO: do NOT complete these file extensions
fignore+=(
  .zwc
  .old
  .tmp
  .bak)
# compinit }}}


# inputrc {{{
#function () {
#  local inputrc=${INPUTRC:-$HOME/.inputrc}
#  [[ -f $inputrc ]] && source "$inputrc"
#}
# }}}


# 3rd-party {{{
# FIXME: should be shell agnostic
# INFO: ~/.config/bash_completion
function() {
  local bash_completion="${XDG_CONFIG_HOME:-$HOME/.config}/bash_completion"
  [[ -r $bash_completion ]] && source "$bash_completion"
}

function() {
  local pip_complete_file="$HOME/.zsh/site-functions/_pip"
  if (( $+commands[pip] )) && [[ -f $pip_complete_file ]] \
    source "$pip_complete_file"
}

# INFO: https://github.com/tamago324/compe-zsh
function() {
  typeset -gx COMPE_ZSH_CACHE_DIR=${COMPE_ZSH_CACHE_DIR:-"${XDG_CACHE_HOME:-"$HOME/.cache"}/compe/zsh"}
  local compe_cache="$COMPE_ZSH_CACHE_DIR"
  if [ -d $compe_cache ]; then
    export FPATH="${compe_cache}:${FPATH}"
  fi
}

# INFO: https://github.com/lincheney/fzf-tab-completion#zsh
function() {
  if [[ ${LINUX:-0} == ${TRUE:-1} ]]; then
    local fzf_tab_completion_f="/usr/share/fzf-tab-completion/zsh/fzf-zsh-completion.sh"
  elif [[ ${OSX:-0} == ${TRUE:-1} ]]; then
    local fzf_tab_completion_f="$HOME/src/github.com/lincheney/fzf-tab-completion/zsh/fzf-zsh-completion.sh"
  fi
  if [[ -f $fzf_tab_completion_f ]]; then
    source $fzf_tab_completion_f
    function smart_completion() {
      if [[ "$LBUFFER" =~ [a-zA-Z0-9]$ ]]; then
        _main_complete
      else
        fzf_completion
      fi
    }
    zle -C fzf_completion complete-word smart_completion

    #bindkey '^I' fzf_completion

    zstyle ':completion:*' fzf-search-display true

    zstyle ':completion::*:(-command-):*'                     fzf-search-display true
    zstyle ':completion::*:(-command-):*'                     fzf-completion-opts --preview='eval eval echo {1}'
    zstyle ':completion::*:(-parameter-|-brace-parameter-):*' fzf-search-display true
    zstyle ':completion::*:(-parameter-|-brace-parameter-):*' fzf-completion-opts --preview='eval eval echo {1}'
    zstyle ':completion::*:(export|unset|expand):*'           fzf-search-display true
    zstyle ':completion::*:(export|unset|expand):*'           fzf-completion-opts --preview='eval eval echo {1}'

    zstyle ':completion:*:*:ls:*'                             fzf-search-display true
    zstyle ':completion::*:ls::*'                             fzf-completion-opts --preview='eval head {1}'

    zstyle ':completion:*:*:cd:*'                             fzf-search-display true

    zstyle ':completion:*:*:git:*'                            fzf-search-display true
    zstyle ':completion::*:git::git,add,*'                    fzf-search-display true
    # preview a `git status` when completing git add
    zstyle ':completion::*:git::git,add,*'                    fzf-completion-opts --preview='git -c color.status=always status --short'
    # if other subcommand to git is given, show a git diff or git log
    zstyle ':completion::*:git::*,[a-z]*'                     fzf-completion-opts --preview='
    eval set -- {+1}
    for arg in "$@"; do
      { git diff --color=always -- "$arg" | git log --color=always "$arg" } 2>/dev/null
      done'
  fi
}
# 3rd-party }}}
