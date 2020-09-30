#!/usr/bin/env zsh
# vim: set fdm=marker:


# NOTE: run this BEFORE sourcing 3rd-party completions
# compinit {{{
autoload -Uz +X bashcompinit && bashcompinit
autoload -Uz +X compinit
function() {
  local zcompdump=$ZDOTDIR/.zcompdump
  if [[ $OSX == $TRUE && -d $BREW/opt/coreutils ]] && alias date=gdate
  if [[ -f $zcompdump && $(date +'%j') > $(date +'%j' -r $zcompdump) ]]; then
    compinit
  else
    compinit -C
  fi
  alias date >/dev/null && unalias date
}

zmodload -i zsh/complist

set completion-ignore-case on
set show-all-if-ambiguous on

# INFO: do NOT complete these file extensions
fignore+=(
  .zwc
  .old
  .tmp
  .bak)
# compinit }}}


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


# 3rd-party {{{
if command_exists kitty; then
  kitty + complete setup zsh | source /dev/stdin
fi
# 3rd-party }}}
