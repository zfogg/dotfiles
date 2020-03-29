#!/usr/bin/env zsh
# vim: set fdm=marker:
# ~/.zshrc


# init: zsh modules, autoload, antigen-hs {{{
zmodload zsh/zpty

autoload -Uz          \
    select-word-style \
    zsh-mime-setup
select-word-style normal
zsh-mime-setup

export ANTIGEN_HS_SANDBOX='stack'
source ~/.zsh/antigen-hs/init.zsh
# }}}


# $ZDOTDIR/z/ {{{
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
    [[ -f $BREW/etc/grc.bashrc ]] && source "$BREW/etc/grc.bashrc"
  }
fi
# }}}


# plugins {{{
#if command_exists direnv; then
  #export AUTOENV_DISABLED=0
  #export AUTOENV_FILE_ENTER=.env
  #export AUTOENV_HANDLE_LEAVE=0
  #export AUTOENV_LOOK_UPWARDS=0
  #eval "$(direnv hook zsh)"
#fi

[[ -f $BREW/etc/profile.d/z.sh ]] && command_exists z \
  && source "$BREW/etc/profile.d/z.sh"

if command_exists fzf; then
  function() {
    [[ -f $HOME/.fzf.zsh ]] && source "$HOME/.fzf.zsh"
    local fzfetc=("$BREW/share/fzf/"{completion,key-bindings}.zsh)
    find "${fzfetc[@]}" &>/dev/null \
      && source "${fzfetc[@]}"
  }
fi

function() { # grep, rg
  if command_exists rg; then
    export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"
    local RG_PRG='rg'
    local GREPPRG_PRG="$RG_PRG"
    local GREPPRG_ARGS="$RG_ARGS"
    export FZF_DEFAULT_COMMAND="$RG_PRG --vimgrep "

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

  export GENCOMPL_FPATH="$HOME/.zsh/complete"
  if [[ -d $GENCOMPL_FPATH ]]; then
    source ~/.zsh/zsh-completion-generator/zsh-completion-generator.plugin.zsh
  else
    unset GENCOMPL_FPATH
  fi
}


# node, npm, nvm {{{
function nvmRC() {
  unset -f "$1"
  unset npm_config_prefix
  export NVM_DIR="$HOME/.nvm"
  [[ -s $NVM_DIR/nvm.sh && ! -v NVM_CD_FLAGS ]] \
    && source "$NVM_DIR/nvm.sh" 
}
#function node() { nvmRC node; node "$@"; }
#function npm()  { nvmRC npm;  npm  "$@"; }
#function nvm()  { nvmRC nvm;  nvm  "$@"; }
# node, npm, nvm }}}

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


# zsh startup debug (BOTTOM of ~/.zshrc) {{{
#   https://kev.inburke.com/kevin/profiling-zsh-startup-time
if [[ ! -z $SHELL_DEBUG ]]; then
  unsetopt xtrace
  exec 2>&3 3>&-
fi
# zsh startup debug (BOTTOM of ~/.zshrc) }}}
