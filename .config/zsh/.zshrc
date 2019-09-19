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

# completions, complist, compinit {{{
source "$ZDOTDIR/z/complete.zsh"
set completion-ignore-case on
set show-all-if-ambiguous on
zmodload zsh/complist
autoload -Uz +X compinit
for dump in "$ZDOTDIR"/.zcompdump(N.mh+24); do
  compinit
done
compinit -C
# completions, listings }}}

# aliases, functions
source "$HOME/.aliases"
# $ZDOTDIR/z/ }}}



# terminfo, iterm integration {{{
#local dot_ti=~/.terminfo/"$TERM".ti
#[[ -f "$dot_ti" ]] \
#    || infocmp "$TERM" \
#    | sed 's/kbs=^[hh]/kbs=\\177/' \
#    > "$dot_ti"
#tic "$dot_ti"

if [[ "${OSX:-0}" == "${TRUE:-1}" && -v ITERM_SESSION_ID ]]; then
  function() {
    local iterm2_integration=~/.iterm2_shell_integration."${SHELL_NAME:t}"
    if [ -f "$iterm2_integration" ]; then
      source "$iterm2_integration"
      # INFO: github.com/garabik/grc
      source "${BREW}/etc/grc.bashrc"
    fi
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

command_exists z || [ -f "${BREW}/etc/profile.d/z.sh" ] \
    && source "${BREW}/etc/profile.d/z.sh"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
command_exists fzf \
  && [ -f "/usr/share/fzf/key-bindings.zsh" ] \
  && [ -f "/usr/share/fzf/completion.zsh" ] \
    && source /usr/share/fzf/{key-bindings,completion}.zsh

function() {
  # grep, rg
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

export GENCOMPL_FPATH="${HOME}/.zsh/complete"
if [[ -d "$GENCOMPL_FPATH" ]]; then
  source ~/.zsh/zsh-completion-generator/zsh-completion-generator.plugin.zsh
else
  unset GENCOMPL_FPATH
fi
# plugins }}}


# arch, pacman, paccache {{{
if [[ "${LINUX:-0}" == "${TRUE:-1}" ]]; then
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
fi
# }}}


# zsh startup debug (BOTTOM of ~/.zshrc) {{{
#   https://kev.inburke.com/kevin/profiling-zsh-startup-time
if [[ ! -z "$SHELL_DEBUG" ]]; then
  unsetopt xtrace
  exec 2>&3 3>&-
fi
# zsh startup debug (BOTTOM of ~/.zshrc) }}}
