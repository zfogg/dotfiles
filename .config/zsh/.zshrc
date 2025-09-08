#!/usr/bin/env zsh
# vim: set fdm=marker:
#   ~/.config/zsh/.zshrc


# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.config/zsh/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi


# init: zsh modules {{{
zmodload -a zsh/zpty zpty
#zmodload -a zsh/stat stat
#zmodload -a zsh/zprof zprof
#zmodload -a zsh/mapfile mapfile

# zsh builtins
autoload -Uz          \
    select-word-style \
    zsh-mime-setup
select-word-style normal
zsh-mime-setup

export ANTIGEN_HS_HOME="$HOME/.zsh/antigen-hs"
export ANTIGEN_HS_MY="$ANTIGEN_HS_HOME/../MyAntigen.hs"
export ANTIGEN_HS_OUT="$HOME/.antigen-hs"
export ANTIGEN_HS_SANDBOX='stack'
#export ANTIGEN_HS_SANDBOX='cabal'
source "$HOME/.zsh/antigen-hs/init.zsh"
# }}}


# $ZDOTDIR/z/ {{{
# path, manpath, fpath, infopath
source "$ZDOTDIR/z/path.zsh"

# bindkey, zle
source "$ZDOTDIR/z/keys.zsh"

# history
source "$ZDOTDIR/z/history.zsh"

# ls colors, highlighting
source "$ZDOTDIR/z/theme.zsh"

# completions, complist, compinit, compdef
source "$ZDOTDIR/z/complete.zsh"
# $ZDOTDIR/z/ }}}


# plugins {{{
#if command_exists direnv; then
  export AUTOENV_DISABLED=0
  export AUTOENV_FILE_ENTER=.env
  export AUTOENV_HANDLE_LEAVE=0
  export AUTOENV_LOOK_UPWARDS=0
  #emulate zsh -c "$(direnv hook zsh)"
#fi

function() {
  local asdf_dir=${ASDF_DIR:-/dev/null}
  local asdf_sh=${asdf_dir}/asdf.sh
  if [[ -d $asdf_dir && -f $asdf_sh ]]; then
    source $asdf_sh;
    eval "$(asdf exec direnv hook zsh)"
  fi
  if [[ -f ${XDG_CONFIG_HOME:-$HOME/.config}/asdf-direnv/zshrc ]]; then
    source "${XDG_CONFIG_HOME:-$HOME/.config}/asdf-direnv/zshrc"
  fi
}

#if command_exists jenv; then
#  #eval "$(jenv init -)"
#fi

# INFO: antigen handles this now
#[[ -f $BREW/etc/profile.d/z.sh ]] && command_exists z \
  #&& source "$BREW/etc/profile.d/z.sh"

local RG_PRG='rg '
local RG_ARGS="--column --line-number --no-heading --sortr=modified --ignore --hidden --color=always --smart-case -g'!.git'"
function() { # grep, rg
  if command_exists rg; then
    export RIPGREP_CONFIG_PATH="$HOME/.dotfiles/.ripgreprc"
    local GREPPRG_PRG="$RG_PRG"
    local GREPPRG_ARGS="$RG_ARGS"
    export FZF_DEFAULT_COMMAND="$RG_PRG --files $RG_ARGS 2>/dev/null "

  else # INFO: "command_exists grep; then"
    local GREP_PRG='grep '
    local GREP_ARGS='--color=auto'
    GREP_ARGS+=' --exclude=\*.{o,pyc,.min.js}'
    GREP_ARGS+=' --exclude-dir={.bzr,cvs,.git,.hg,.svn,node_modules}'
    local GREPPRG_PRG="$GREP_PRG"
    local GREPPRG_ARGS="$GREP_ARGS"
    export FZF_DEFAULT_COMMAND="$GREP_PRG $GREP_ARGS -g "
  fi

  [[ -v GREPPRG_PRG && -v GREPPRG_ARGS ]] \
    && export GREPPRG="${GREPPRG_PRG}${GREPPRG_ARGS}" \
    || export GREPPRG="${GREPPRG:-command -p grep}"
}

if command_exists fzf; then

  if `command_exists fd`; then
    # Use fd (https://github.com/sharkdp/fd) for listing path candidates.
    _fzf_compgen_path() {
      fd --hidden --follow --exclude ".git" . "$1"
    }
    # Use fd to generate the list for directory completion
    _fzf_compgen_dir() {
      fd --type d --hidden --follow --exclude ".git" . "$1"
    }
  fi
  function() {
    export FZF_HISTORY_DIR="${XDG_DATA_HOME:-${HOME}/.fzf}/fzf"
    [[ -d $FZF_HISTORY_DIR ]] || mkdir -p "$FZF_HISTORY_DIR"
    export FZF_HISTORY_FILE="${FZF_HISTORY_DIR}/history"
    export FZF_COMPLETION_TRIGGER=';;'
    export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
    export FZF_CTRL_T_OPTS="
      --walker-skip .git,node_modules,target
      --preview 'fzf-preview.sh {}'
      --bind 'ctrl-/:change-preview-window(down|hidden|)'
      --bind=ctrl-d:half-page-down
      --bind=ctrl-u:half-page-up
    "
    export FZF_CTRL_Z_KEYBINDS="
      --bind=ctrl-n:down
      --bind=ctrl-p:up
      --bind=ctrl-d:half-page-down
      --bind=ctrl-u:half-page-up
      --bind=ctrl-space:toggle-preview
      --bind=ctrl-a:toggle-all
    "
    export FZF_CTRL_R_OPTS="
      --multi
      ${FZF_CTRL_Z_KEYBINDS}
    "
      #--extended-exact \
    export FZF_DEFAULT_OPTS="
      --ansi
      --layout=reverse
      --info=inline
      --height=100%
      --multi
      --history=${FZF_HISTORY_FILE}
      --history-size=10000
      ${FZF_CTRL_Z_KEYBINDS}
    "
    export FZF_COMPLETION_PATH_OPTS='--walker file,dir,follow,hidden'
    export FZF_COMPLETION_DIR_OPTS='--walker dir,follow,hidden'

    local command_tree='(command tree --du --si --filelimit 512 -I "node_modules|build|.git|.venv|env|__pycache__" -L 5 -C {}) 2>/dev/null | head -200'

    function _fzf_comprun() {
      local command=$1
      shift
      local command_tree='(command tree --du --si --filelimit 512 -I "node_modules|build|.git|.venv|env|__pycache__" -L 5 -C {}) 2>/dev/null | head -200'
      case "$command" in
        cd)                fzf --preview "$command_tree"                        "$@" ;;
        export|unset|echo) fzf --preview "eval 'echo \$'{}"                     "$@" ;;
        ssh)               fzf --preview 'dog {}'                               "$@" ;;
        *)                 fzf --preview 'fzf-preview.sh {} || $command_tree'   "$@" ;;
      esac
    }

    # FZF_ALT_C_COMMAND
    if command_exists fd; then
      export FZF_ALT_C_COMMAND="fd --type d --hidden --follow --exclude '.git'"
    fi
    export FZF_ALT_C_OPTS="
      --preview '$command_tree'
      ${FZF_CTRL_Z_KEYBINDS}
    "

    # [[ -f ~/.fzf.zsh ]] && source ~/.fzf.zsh # INFO: this was for vim but we already do the same thing in the line below here.
    source <(fzf --zsh)

    # / ALT-C -> CTRL-G
    bindkey -r '\ec'
    bindkey '^G' fzf-cd-widget

    # / CTRL-T -> CTRL-P
    bindkey -r '^T'
    bindkey -r '^F'
    bindkey -v '^F' fzf-file-widget
  }
  function vo() {
    fzf --preview='fzf-preview.sh {}' --multi --bind 'enter:become(nvim {+})'
  }
  function vf() {
    eval "$GREPPRG" . | fzf --delimiter : --nth 3.. --bind 'enter:become(nvim {1} +{2})'
  }
  # fkill - kill processes - list only the ones you can kill. Modified the earlier script.
  function fkill() {
    local pid
    local pids
    if [ "$UID" != "0" ]; then
      local pids=$(ps aux -r -U $UID)
    else
      local pids=$(ps aux -r)
    fi
    local pid=$(echo $pids \
      | awk '{printf "%-8s %5s %5s %5s %10s %10s %s\n", $2, $1, $3, $4, $9, $10, substr($0, index($0,$11))}' \
      | fzf --multi --preview 'ps o args {1} | sed 1d; ps mu {1} | awk '\''{printf "%-8s %5s %5s %5s %10s %10s\n", $2, $1, $3, $4, $9, $10}'\' \
      | awk '{print $2}')
    if [ "x$pid" != "x" ]
    then
        echo $pid | xargs kill -${1:-9}
    fi
  }
fi


# node, npm, nvm {{{
#function nvmRC() {
  #unset -f "$1"
  #unset npm_config_prefix
  #export NVM_DIR="$HOME/.nvm"
  #[[ -s $NVM_DIR/nvm.sh && ! -v NVM_CD_FLAGS ]] \
    #&& source "$NVM_DIR/nvm.sh"
#}
#function node() { nvmRC node; node "$@"; }
#function npm()  { nvmRC npm;  npm  "$@"; }
#function nvm()  { nvmRC nvm;  nvm  "$@"; }
#function npx()  { nvmRC npx;  npx  "$@"; }
# node, npm, nvm }}}

# python, pip, pyenv {{{
#function() {
  #if [[ -d $PYENV_ROOT ]] && command_exists pyenv && command_exists pyenv-virtualenv; then
    #eval "$(pyenv            init -)"
    #eval "$(pyenv virtualenv-init -)"
    #source "$BREW/opt/pyenv/completions/pyenv.zsh"
  #fi
#}

#function workon_home() {
#  local _workon_home="${WORKON_HOME:-~/.virtualenvs}"
#  echo "$_workon_home"
#}
#
#function project_home() {
#  local _project_home="${PROJECT_HOME:-${1:-${GHQ_ROOT:-~/src}}}"/"${2:-github.com}"
#  echo "$_workon_home"
#}
#
#function mkenv() {
#  local _workon_home="$(workon_home)"
#  mkdir -p "$_workon_home"
#  `asdf where python "$1"`/bin/virtualenv \
#    --seeder pip \
#    --download \
#    -p $(asdf where python "$1")/bin/python "$_workon_home"/"$2"
#  #--system-site-packages \
#  #--symlink-app-data \
#}
#
#function workon() {
#  source "$(workon_home)"/"$1"/bin/activate
#  #[ -d "$(project_home)"/"$1" ] && cd "$(project_home)"/"$1"
#}

# INFO: https://vi.stackexchange.com/questions/7644/use-vim-with-virtualenv/7654#7654
#if [[ -n $VIRTUAL_ENV && -e "${VIRTUAL_ENV}/bin/activate" ]]; then
#  source "${VIRTUAL_ENV}/bin/activate"
#fi
# python, pip, pyenv }}}
# plugins }}}


# terminfo, iTerm2 integration {{{
function() {
  if [[ -v TERM && $TERM != "" ]]; then
    local dot_terminfo="$HOME/.terminfo"
    local first_char_hex=$(printf "%02x" "'${TERM[1]}")
    local term_dot_ti="$dot_terminfo/$first_char_hex/$TERM.ti"
    if [[ ! -f $term_dot_ti ]]; then
      mkdir -p "$dot_terminfo/$first_char_hex"
      infocmp "$TERM" > "$term_dot_ti"
      [[ $? == 0 ]] \
        && tic "$term_dot_ti" \
        || 1>&2 echo "tic error - \$TERM='$TERM'"
    fi
  fi
}

if [[ ${OSX:-0} == ${TRUE:-1} && -v ITERM_SESSION_ID ]]; then
  function() {
    local iterm2_integration="$HOME/.iterm2_shell_integration.${SHELL_NAME:t}"
    [[ -f $iterm2_integration  ]] && source "$iterm2_integration"
    # INFO: github.com/garabik/grc
    #[[ -f $BREW/etc/grc.bashrc ]] && source "$BREW/etc/grc.bashrc"
  }
fi
# }}}


# arch, pacman, paccache {{{
if [[ ${LINUX:-0} == ${TRUE:-1} ]]; then
  autoload -Uz add-zsh-hook
  zshcache_time="$(date +%s%N)"
  function rehash_precmd() {
    if [[ -a /var/cache/zsh/pacman ]]; then
      local paccache_time=$(date -r /var/cache/zsh/pacman +%s%N)
      if (( zshcache_time < paccache_time )); then
        rehash
        zshcache_time="$paccache_time"
      fi
    fi
  }
  add-zsh-hook -Uz precmd rehash_precmd
fi
# }}}


# aliases, functions {{{
source "$HOME/.aliases"
# }}}


# make {{{
if [[ $OSX == $TRUE ]]; then
  export MAKEFLAGS="-j `nproc`"
fi
# make }}}


# gitstatus {{{
#function() {
  #local gitstatus_source=/dev/null
  #if [[ $OSX == $TRUE ]]; then
    #gitstatus_source=$BREW/opt/gitstatus/gitstatus.prompt.zsh
  #elif [[ $LINUX == $TRUE ]]; then
    #gitstatus_source=/usr/share/gitstatus/gitstatus.prompt.zsh
  #fi
  #[[ -f $gitstatus_source ]] && source $gitstatus_source
#}
# }}}


# powerlevel10k {{{
function() {
  local pl10k_file=powerlevel10k.zsh-theme
  local pl10k_root=
  if [[ $OSX == $TRUE ]]; then
    pl10k_root=$BREW/opt/powerlevel10k
  elif [[ $LINUX == $TRUE ]]; then
    pl10k_root=/usr/share/zsh-theme-powerlevel10k
  fi
  local pl10k_path=$pl10k_root/$pl10k_file
  [[ -d $pl10k_root && -f $pl10k_path ]] && source "$pl10k_path"
  [[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh
}
# }}}


# crypto auth agents {{{
#function() {
#  return
#  local sfa_script=$(which ssh-find-agent.sh)
#  [[ -x "$sfa_script" ]] && source "$sfa_script"
#  ssh_find_agent -a || \
#    eval $(ssh-agent) > /dev/null
#  pgrep gpg-agent >/dev/null || \
#    gpg-agent --daemon --default-cache-ttl 1728000 --max-cache-ttl 1728000
#  export GPG_AGENT_INFO=$(gpgconf --list-dirs agent-socket)
#  export GPG_TTY=$(tty)
#
#  #export SSH_AUTH_SOCK=/Users/zfogg/Library/Containers/org.hejki.osx.sshce.agent/Data/socket.ssh
#  #local keys="${(f)ssh_keys}"
#  #local ssh_keys="$(for x in `\ls ~/.ssh/id_rsa*~*.pub`; basename "$x")"
#  #export GPG_TTY="$(tty)"
#  # INFO: ^ moved to ~/.zshenv so p10k won't make me not a TTY
#  # INFO: https://unix.stackexchange.com/a/608843/99026
#  if command_exists keychain; then
#    typeset -a keys=()
#    local gpg_keyid_file="$HOME"/.gnupg/.keyid
#    local ssh_keyid_file="$HOME"/.ssh/.keyid
#    if [[ -f $gpg_keyid_file ]] { keys+=$(cat "$gpg_keyid_file") }
#    if [[ -f $ssh_keyid_file ]] { keys+="$ssh_keyid_file" }
#    local life=28800
#    #--systemd \
#    #--noask \
#    eval $(keychain \
#      --dir "${XDG_CACHE_HOME:-~/.cache}"/.keychain \
#      --confhost \
#      --eval \
#      --timeout "$life" \
#      --agents ssh,gpg \
#      --quick \
#      --quiet \
#      --inherit any \
#      "${(@f)keys}")
#  fi
#}
# }}}


# zsh startup debug (BOTTOM of ~/.zshrc) {{{
#   https://kev.inburke.com/kevin/profiling-zsh-startup-time
if [[ ! -z $SHELL_DEBUG ]]; then
  unsetopt xtrace
  exec 2>&3 3>&-
fi
# zsh startup debug (BOTTOM of ~/.zshrc) }}}
#source /etc/nix/nix-profile.sh
#. /Users/zfogg/.nix-profile/etc/profile.d/nix.sh

# heroku autocomplete setup
#HEROKU_AC_ZSH_SETUP_PATH=/home/zfogg/.cache/heroku/autocomplete/zsh_setup && test -f $HEROKU_AC_ZSH_SETUP_PATH && source $HEROKU_AC_ZSH_SETUP_PATH;

# INFO: https://github.com/b4b4r07/emoji-cli
#export EMOJI_CLI_FILTER='fzf-tmux -d 15%:fzf:peco:percol'
#EMOJI_CLI_PATH="$HOME/.antigen-hs/repos/https-COLON--SLASH--SLASH-github.com-SLASH-b4b4r07-SLASH-emoji-cli/emoji-cli.plugin.zsh"
#if [[ -f $EMOJI_CLI_PATH ]]; then
  #export EMOJI_CLI_FILTER='fzf:peco:percol'
  #export EMOJI_CLI_KEYBIND='^s'
  #export EMOJI_CLI_USE_EMOJI=1
  #source "$EMOJI_CLI_PATH"
  #bindkey -r "${EMOJI_CLI_KEYBIND:-^s}"
  #bindkey -v "${EMOJI_CLI_KEYBIND:-^s}" emoji::cli
#fi


# ishan / quackduck: magic
#function() {
  #return
  #local magicf=~/bin/magic.sh
  #if [[ -f $magicf ]]; then
    #source "$magicf"
  #fi
#}

# pnpm
export PNPM_HOME="$HOME/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

export PATH="$BREW/opt/ruby/bin:$PATH"
export PATH="$HOME/.gem/bin:$PATH"

export HOMEBREW_NO_ENV_HINTS=1
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export AWS_PROFILE=softmax
