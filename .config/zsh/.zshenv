#!/usr/bin/env zsh
# vim: set fdm=marker:


#zmodload zsh/zprof

# {{{ zsh startup debug (TOP of ~/.zshenv)
  # INFO: https://kev.inburke.com/kevin/profiling-zsh-startup-time
  #export SHELL_DEBUG=1
  if [[ ! -z "$SHELL_DEBUG" ]]; then
    local zsh_debug_log=$(mktemp -t zsh_debug_log)
    local SHELL_DEBUG_LOG=$(/usr/local/bin/grealpath "$zsh_debug_log")
    function shell-debug-log {
      $EDITOR -u NONE +'1d' +'x' "$SHELL_DEBUG_LOG"
      (echo "# vim: fdm=marker fen:"; cat "$SHELL_DEBUG_LOG") > "$SHELL_DEBUG_LOG".tmp
      mv "$SHELL_DEBUG_LOG".tmp "$SHELL_DEBUG_LOG"
      echo "#}''}}" >> "$SHELL_DEBUG_LOG"
      $EDITOR "$SHELL_DEBUG_LOG"
    }
    zmodload zsh/datetime
    setopt PROMPT_SUBST
    PS4=$'#}'$'}}\012\012\012# %x:%I {'$'{{\012# %N:i \012# +$EPOCHREALTIME\012  '
    #PS4='+$EPOCHREALTIME %N:%i> '
    exec 3>&2 2>$SHELL_DEBUG_LOG
    setopt XTRACE
  fi
# }}} zsh startup debug (TOP of ~/.zshenv)


# note: meta helpers {{{
command_exists() { command -v "$1" 2>/dev/null 1>&2 }
# alias_exists()   { alias      "$1" 2>/dev/null 1>&2 }
export DISABLE_UPDATE_PROMPT=true
# }}}


# $SHELL {{{
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_DATA_HOME="${HOME}/.local/share"

export  TRUE='1'
export FALSE='0'

export OSX="$(
  [[ $OSTYPE =~ "darwin" ]]
  [[ $? == "0" ]] \
    && echo "${TRUE:-1}" \
    || echo "${FALSE-0}")"

export LINUX="$(
  [[ $OSTYPE =~ "linux" ]]
  [[ $? == "0" ]] \
    && echo "${TRUE:-1}" \
    || echo "${FALSE-0}")"

export DOTFILES=~/.dotfiles
export DOTVIM=~/.vim
export MYVIMRC="$DOTVIM/init.vim"

unset MAILCHECK

export PACKAGE_PREFIX="gg.zfo"

export  DOTFILES_SETENV="1"

export ZDOTDIR="${XDG_CONFIG_HOME:-${HOME}/.config}/zsh"
export   ZSHRC="$ZDOTDIR/.zshrc"
unsetopt GLOBAL_RCS

if [[ "$OSX" == "$TRUE" ]]; then
  export BREW='/usr/local'
elif [[ "$LINUX" == "$TRUE" ]]; then
  export BREW='/usr'
else
  export BREW='/usr/local'
fi

export PKG_CONFIG_PATH="/usr/lib/pkgconfig"
if [[ "$OSX" == "$TRUE" ]]; then
  export PKG_CONFIG_PATH="${BREW}/lib/pkgconfig:${PKG_CONFIG_PATH}"
fi

export SHELL_NAME="$(basename "$SHELL")"
# $SHELL }}}


# editor, pager {{{
export EDITOR='nvim'
export VISUAL="$EDITOR"
export MANPAGER="$EDITOR -c 'set ft=man' -"
export LESS='-R'

#export PAGER='nvimpager'
export PAGER='nvim -R +AnsiEsc'
#export PAGER='less'
# editor, pager }}}


# terminal {{{
#export TERM="xterm-256color"
#export COLORTERM="$TERM"
if [[ "$TERM_PROGRAM" == "Apple_Terminal" ]]; then
  export TERMINAL_DOTAPP="true"
  # Correctly display UTF-8 with combining characters:
  setopt combiningchars
elif [[ "$TERM_PROGRAM" == "iTerm.app" ]]; then
  export ITERM_DOTAPP="true"
fi
# terminal }}}


# LC_ {{{
export               LANG=en_US.UTF-8
export           LC_CTYPE=en_US.UTF-8
export         LC_NUMERIC=en_US.UTF-8
export            LC_TIME=en_US.UTF-8
export         LC_COLLATE=en_US.UTF-8
export        LC_MONETARY=en_US.UTF-8
export        LC_MESSAGES=en_US.UTF-8
export           LC_PAPER=en_US.UTF-8
export            LC_NAME=en_US.UTF-8
export         LC_ADDRESS=en_US.UTF-8
export       LC_TELEPHONE=en_US.UTF-8
export     LC_MEASUREMENT=en_US.UTF-8
export  LC_IDENTIFICATION=en_US.UTF-8
export             LC_ALL=en_US.UTF-8
# LC_ }}}


# $SHELL help {{{
#export HELPDIR="${BREW}/share/zsh/help"
unalias    run-help 2>/dev/null
unfunction run-help 2>/dev/null
#autoload -Uz run-help
# $SHELL help }}}


# groovy {{{
#export GROOVY_HOME="$BREW/opt/groovy/libexec"
#
# groovy }}}


# golang {{{
export GOPATH="$HOME/src/go"
export GO111MODULE='auto' # on | off | auto

# NOTE: apparently go can figure out the GOROOT on its own
#if [[ "${OSX:-0}" == "${TRUE:-1}" ]]; then
  #export GOROOT="$BREW/opt/go/libexec"
#elif [[ "${LINUX:-0}" == "${TRUE:-1}" ]]; then
  #export GOROOT='/usr/lib/go'
#fi
# }}}


# mono {{{
#export MONO_GAC_PREFIX="$BREW"
#
# mono }}}


# node {{{
export NVM_DIR="${HOME}/.nvm"
[ -d ~/.local ] \
  && export npm_config_prefix=~/.local \
  || export npm_config_prefix=~/.npm
# node }}}


# python {{{
#export PIP_REQUIRE_VIRTUALENV=true
#if [[ -n $VIRTUAL_ENV && -e "${VIRTUAL_ENV}/bin/activate" ]]; then;
  #source "${VIRTUAL_ENV}/bin/activate"; fi

export WORKON_HOME=~/.virtualenvs

# pyenv {{{
  # NOTE: PYENV_ROOT+PATH are set by pyenv-lazy via antigen
  #export PYENV_SHELL="$SHELL_NAME"
  #export PYENV_ROOT="${HOME}/.pyenv"
  export PYENV_VIRTUALENV_DISABLE_PROMPT='1'
  export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV='true'
  export ZSH_PYENV_LAZY_VIRTUALENV=true
# }}}

# pipenv {{{
  #export PIPENV_SHELL_COMPAT="$TRUE"
  #export PIPENV_VENV_IN_PROJECT="$TRUE"
  #export PIPENV_MAX_DEPTH='4'
# pipenv }}}
# python }}}


# path, manpath, fpath
source "$ZDOTDIR/z/path.zsh"


# rust {{{
export CORES="`nproc`"

export RUSTUP_HOME=~/.rustup
if [[ "${OSX:-0}" == "${TRUE:-1}" ]]; then
  export RUST_SRC_PATH="${RUSTUP_HOME:-~/.rustup}/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"
elif [[ "${LINUX:-0}" == "${TRUE:-1}" ]]; then
  export RUST_SRC_PATH="${RUSTUP_HOME:-~/.rustup}/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"
fi
export CARGO_INCREMENTAL=1
export CARGO_BUILD_JOBS="$((${CORES:-2} - 1))"
# }}}

# zsh-z {{{
export ZSHZ_NO_RESOLVE_SYMLINKS="$TRUE"
export ZSHZ_OWNER="$USER"
# }}}


# compilers {{{
export  CC='clang'
export CXX='clang++'
# }}}
