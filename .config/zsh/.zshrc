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
zmodload -a zsh/stat stat
zmodload -a zsh/zprof zprof
zmodload -a zsh/mapfile mapfile

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


# terminfo, iTerm2 integration {{{
function() {
  if [[ -v TERM && $TERM != "" ]]; then
    local dot_ti="$HOME/.terminfo/$TERM.ti"
    if [[ ! -f $dot_ti ]]; then
      infocmp "$TERM" > "$dot_ti"
      [[ $? == 0 ]] \
        && 2>/dev/null tic "$dot_ti" \
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


# plugins {{{
#if command_exists direnv; then
  export AUTOENV_DISABLED=0
  export AUTOENV_FILE_ENTER=.env
  export AUTOENV_HANDLE_LEAVE=0
  export AUTOENV_LOOK_UPWARDS=0
  #emulate zsh -c "$(direnv hook zsh)"
#fi

function() {
  [[ -f $ASDF_DIR/asdf.sh ]] && source "$ASDF_DIR/asdf.sh"
  if [[ -d $XDG_DATA_HOME/asdf ]]; then
    eval "$(asdf exec direnv hook zsh)"
    #source "$BREW/opt/asdf/asdf.sh"
    #function direnv() { asdf exec direnv "$@"; }

    #local asdf_java=${ASDF_DATA_DIR:-~/.asdf}/plugins/java
    #if [[ -d $asdf_java ]]; then
    #  source "$asdf_java/set-java-home.zsh"
    #fi
  fi
  #path=(
    #$HOME/.yarn/bin
    #"$path[@]")
  #export PATH
}

#if command_exists jenv; then
#  #eval "$(jenv init -)"
#fi

# INFO: antigen handles this now
#[[ -f $BREW/etc/profile.d/z.sh ]] && command_exists z \
  #&& source "$BREW/etc/profile.d/z.sh"

function() { # grep, rg
  if command_exists rg; then
    export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"
    local RG_PRG='rg'
    local GREPPRG_PRG="$RG_PRG"
    local GREPPRG_ARGS="$RG_ARGS"
    export FZF_DEFAULT_COMMAND="$RG_PRG --files --sortr=modified --ignore --hidden 2>/dev/null"

  else # INFO: "command_exists grep; then"
    local GREP_PRG='grep'
    local GREP_ARGS='--color=auto'
    GREP_ARGS+=' --exclude=\*.{o,pyc,.min.js}'
    GREP_ARGS+=' --exclude-dir={.bzr,cvs,.git,.hg,.svn,node_modules}'
    local GREPPRG_PRG="$GREP_PRG"
    local GREPPRG_ARGS=" $GREP_ARGS"
    export FZF_DEFAULT_COMMAND="$GREP_PRG $GREP_ARGS -g "
  fi

  [[ -v GREPPRG_PRG && -v GREPPRG_ARGS ]] \
    && export GREPPRG="${GREPPRG_PRG}${GREPPRG_ARGS}" \
    || export GREPPRG="${GREPPRG:-command -p grep}"
}


if command_exists fzf; then
  _fzf_compgen_path() {
    eval "${FZF_DEFAULT_COMMAND} ${1}"
  }
  _fzf_compgen_dir() {
    eval "${FZF_DEFAULT_COMMAND} ${1} --null | xargs -0 dirname | sort | uniq | tail -n+2"
  }
  function() {
    export FZF_HISTORY_DIR="${XDG_DATA_HOME:-${HOME}/.fzf}/fzf"
    [[ -d $FZF_HISTORY_DIR ]] || mkdir -p "$FZF_HISTORY_DIR"
    export FZF_HISTORY_FILE="${FZF_HISTORY_DIR}/history"
    export FZF_COMPLETION_TRIGGER=';;'
    export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
    export FZF_CTRL_T_OPTS="
      --preview '(bat --color=always --paging=never --number {} || tree -C {}) 2>/dev/null | head -200'
    "
    export FZF_FILE_PREVIEW_OPT="(bat --color=always --paging=never --number {} || tree -C {}) 2>/dev/null | head -200"
    export FZF_CTRL_Z_KEYBINDS="
    --bind=ctrl-d:half-page-down \
    --bind=ctrl-u:half-page-up \
    --bind=ctrl-space:toggle-preview \
    "
    export FZF_CTRL_R_OPTS="
      --no-preview \
      --multi \
      --history=${FZF_HISTORY_FILE} \
      ${CTRL_Z_KEYBINDS}
      --bind=alt-n:down \
      --bind=alt-p:up \
    "
      #--extended-exact \
    export FZF_DEFAULT_OPTS="
      --ansi \
      --layout=reverse \
      --info=inline \
      --height=100% \
      --multi \
      ${CTRL_Z_KEYBINDS}
      --bind ctrl-u:toggle-all
      --no-mouse \
    "
    #if command_exists fd; then
      #export FZF_DEFAULT_COMMAND='fd --type f'
    #fi
    [[ -f ~/.fzf.zsh ]] && source ~/.fzf.zsh
    #local fzfetc=("$BREW/share/fzf/"{completion,key-bindings}.zsh)
    #find "${fzfetc[@]}" &>/dev/null \
      #&& source "${fzfetc[@]}"

    # FZF_ALT_C_COMMAND
    if command_exists fd; then
      export FZF_ALT_C_COMMAND='fd --type d'
    fi
    export FZF_ALT_C_OPTS="
      --preview '(tree -C {}) 2>/dev/null | head -200'
    "
    # / ALT-C -> CTRL-G
    bindkey -r '\ec'
    bindkey '^G' fzf-cd-widget

    # / CTRL-T -> CTRL-P
    bindkey -r '^T'
    bindkey -r '^F'
    bindkey -v '^F' fzf-file-widget
  }
  function vo() {
    $EDITOR -o "`rgf | fzf --preview=$FZF_FILE_PREVIEW_OPT`"
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
function() {
  local sfa_script=$(which ssh-find-agent.sh)
  [[ -x "$sfa_script" ]] && source "$sfa_script"
  ssh_find_agent -a || \
    eval $(ssh-agent) > /dev/null
  pgrep gpg-agent >/dev/null || \
    gpg-agent --daemon --default-cache-ttl 1728000 --max-cache-ttl 1728000
  export GPG_AGENT_INFO=$(gpgconf --list-dirs agent-socket)
  export GPG_TTY=$(tty)
}

function() {
  #export SSH_AUTH_SOCK=/Users/zfogg/Library/Containers/org.hejki.osx.sshce.agent/Data/socket.ssh
  #local keys="${(f)ssh_keys}"
  #local ssh_keys="$(for x in `\ls ~/.ssh/id_rsa*~*.pub`; basename "$x")"
  #export GPG_TTY="$(tty)"
  # INFO: ^ moved to ~/.zshenv so p10k won't make me not a TTY
  # INFO: https://unix.stackexchange.com/a/608843/99026
  if command_exists keychain; then
    typeset -a keys=()
    local gpg_keyid_file="$HOME"/.gnupg/.keyid
    local ssh_keyid_file="$HOME"/.ssh/.keyid
    if [[ -f $gpg_keyid_file ]] { keys+=$(cat "$gpg_keyid_file") }
    if [[ -f $ssh_keyid_file ]] { keys+="$ssh_keyid_file" }
    local life=28800
    #--systemd \
    #--noask \
    #--quick \
    eval $(keychain \
      --dir "${XDG_CACHE_HOME:-~/.cache}"/.keychain \
      --confhost \
      --eval \
      --timeout "$life" \
      --agents ssh,gpg \
      --quiet \
      --inherit any \
      "${(@f)keys}")
  fi
}
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

# tabtab source for packages
# uninstall by removing these lines
[[ -f ~/.config/tabtab/zsh/__tabtab.zsh ]] && . ~/.config/tabtab/zsh/__tabtab.zsh || true

# heroku autocomplete setup
HEROKU_AC_ZSH_SETUP_PATH=/home/zfogg/.cache/heroku/autocomplete/zsh_setup && test -f $HEROKU_AC_ZSH_SETUP_PATH && source $HEROKU_AC_ZSH_SETUP_PATH;

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
function() {
  local magicf=~/bin/magic.sh
  if [[ -f $magicf ]]; then
    source "$magicf"
  fi
}
