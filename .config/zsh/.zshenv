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
    zmodload -a zsh/datetime datetime
    setopt PROMPT_SUBST
    PS4=$'#}'$'}}\012\012\012# %x:%I {'$'{{\012# %N:i \012# +$EPOCHREALTIME\012  '
    #PS4='+$EPOCHREALTIME %N:%i> '
    exec 3>&2 2>$SHELL_DEBUG_LOG
    setopt XTRACE
  fi
# }}} zsh startup debug (TOP of ~/.zshenv)


# shell crosscompat {{{
# .profile
export DOTPROFILE="${HOME}/.profile"
[[ -f $DOTPROFILE ]] && source "$DOTPROFILE"
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

if [[ $OSX == $TRUE ]]; then
  export BREW='/usr/local'
  export HOMEBREW_CLEANUP_MAX_AGE_DAYS='2'
elif [[ $LINUX == $TRUE ]]; then
  export BREW='/usr'
else
  export BREW='/usr/local'
fi

export  LDFLAGS=''
export CPPFLAGS=''
export PKG_CONFIG_PATH='/usr/lib/pkgconfig'
if [[ $OSX == $TRUE ]]; then
  export PKG_CONFIG_PATH="$BREW/lib/pkgconfig:$PKG_CONFIG_PATH"

  export LIBRARY_PATH="/usr/lib:$BREW/lib"
  #if [[ ! -v LIBRARY_PATH ]]; then
    #export LIBRARY_PATH="$LIBRARY_PATH:$BREW/lib"
  #else
    #export LIBRARY_PATH="/usr/lib:$BREW/lib"
  #fi
fi

# function() {
#   local NCURSES="${BREW}/opt/ncurses"
#   if [[ -d $NCURSES ]]; then
#     export PKG_CONFIG_PATH="${NCURSES}/lib/pkgconfig:${PKG_CONFIG_PATH}"
#     export  LDFLAGS="-L${NCURSES}/lib $LDFLAGS"
#     export CPPFLAGS="-I${NCURSES}/include $CPPFLAGS"
#   fi
# }

export SHELL_NAME=`current_shell`

# $SHELL }}}


# terminal {{{
if [[ -v TMUX ]]; then
  #export TERM="screen-256color"
fi
if [[ -v TERM ]]; then
  export COLORTERM="truecolor"
fi
if [[ "$TERM_PROGRAM" == "Apple_Terminal" ]]; then
  export TERMINAL_DOTAPP="true"
  # Correctly display UTF-8 with combining characters:
  setopt combiningchars
elif [[ "$TERM_PROGRAM" == "iTerm.app" ]]; then
  export ITERM_DOTAPP="true"
fi
# terminal }}}


# git {{{
# FIXME: encrypt this 
#export HOMEBREW_GITHUB_API_TOKEN='secret! 🕵'

# INFO: https://git-quick-stats.sh/
export _GIT_PATHSPEC=':!package-lock.json:!yarn.lock'
export _GIT_MERGE_VIEW="enable"
#export _GIT_MERGE_VIEW="exclusive"
# }}}


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
export HELPDIR="${BREW}/share/zsh/help"
autoload -Uz run-help
unalias      run-help 2>/dev/null
unfunction   run-help 2>/dev/null
alias        run-help help

# INFO: https://unix.stackexchange.com/q/214296/99026
#autoload -U run-help
#autoload run-help-git
#autoload run-help-svn
#autoload run-help-svk
#unalias run-help
#alias help=run-help
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
export NVM_LAZY_LOAD=true # lukechilds/zsh-nvm
export NVM_NO_USE=false   # lukechilds/zsh-nvm
#unset NVM_LAZY_LOAD
#unset NVM_NO_USE
export NVM_AUTO_USE=true  # lukechilds/zsh-nvm
#[ -d ~/.local ] \
  #&& export npm_config_prefix=~/.local \
  #|| export npm_config_prefix=~/.npm
# node }}}


# ruby {{{
export RUBY_CONFIGURE_OPTS="--with-openssl-dir=${BREW}/opt/openssl@1.1"
# ruby }}}


# python {{{
export PIP_REQUIRE_VIRTUALENV=true
#if [[ -n $VIRTUAL_ENV && -e "${VIRTUAL_ENV}/bin/activate" ]]; then;
  #source "${VIRTUAL_ENV}/bin/activate"; fi

export WORKON_HOME=~/.virtualenvs

# pyenv {{{
  # NOTE: PYENV_ROOT+PATH are set by pyenv-lazy via antigen
  export PYENV_SHELL="$SHELL_NAME"
  export PYENV_ROOT="$HOME/.pyenv"
  export PYENV_VIRTUALENV_DISABLE_PROMPT='0'
  export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV='true'
  export ZSH_PYENV_LAZY_VIRTUALENV=true
# }}}

# pipenv {{{
  #export PIPENV_SHELL_COMPAT="$TRUE"
  #export PIPENV_VENV_IN_PROJECT="$TRUE"
  #export PIPENV_MAX_DEPTH='4'
# pipenv }}}
# python }}}


# android {{{
if [ -d ~/Library/Android/sdk ]; then
  export ANDROID_HOME=~/Library/Android/sdk
  export ANDROID_SDK_ROOT="$ANDROID_HOME"
  export ANDROID_SDK_VERSION="29.0.3"
elif [ -d /opt/android-sdk ]; then
  export ANDROID_HOME="/opt/android-sdk"
  export ANDROID_SDK_ROOT="$ANDROID_HOME"
  export ANDROID_SDK_VERSION="29.0.3"
fi

function() {
  #for jbt in ant maven gradle; do
    #local jbt_home="$BREW/opt/$jbt"
    #if [[ -d $jbt_home ]]; then
      ##export "${jbt:u}"_HOME="$jbt_home"
    #fi
  #done
}
# android }}}


# path, manpath, fpath {{{
source "$ZDOTDIR/z/path.zsh"
  # note: meta helpers {{{
  #command_exists() { command -v "$1" 2>/dev/null 1>&2 }
  #alias_exists()   { alias      "$1" 2>/dev/null 1>&2 }
  # }}}
# path, manpath, fpath }}}


# editor, pager {{{
if command_exists nvim; then
  export NVIM='nvim'
  export EDITOR="$NVIM"
elif command_exists vim; then
  export VIM='vim'
  export EDITOR="$VIM"
else
  export EDITOR='nano'
fi

if [[ -v NVIM || -v VIM ]]; then
  export MANPAGER="$EDITOR -c 'set ft=man' -"
fi

export VISUAL="$EDITOR"
export LESS='-R'

if command_exists nvimpager; then
  export PAGER='nvimpager'
elif command_exists page; then
  export PAGER="page -q 90000"
elif command_exists nvim; then
  export PAGER='nvim -R +AnsiEsc'
else
  export PAGER='less'
fi
# editor, pager }}}


# rust {{{
export CORES="`$BREW/bin/nproc`"

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


# bash-completion, bashcompinit {{{
export BASH_COMPLETION_USER_FILE="${XDG_CONFIG_HOME:-$HOME/.config}/bash_completion"
export BASH_COMPLETION_USER_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/bash-completion"
# }}}


# zsh-completion-generator {{{
export GENCOMPL_FPATH="$HOME/.zsh/completions"
export GENCOMPL_PY='python2'
# }}}


# ghq {{{
export GHQ_ROOT="$HOME/src"
# ghq }}}


# gpg {{{
export GPG_TTY="$(tty)"
# gpg }}}


# curl {{{
export CURL_HOME="$XDG_CONFIG_HOME/curl"
# curl }}}


# asdf {{{
if [[ -d $XDG_CONFIG_HOME/asdf ]]; then
  export ASDF_CONFIG_DIR="$XDG_CONFIG_HOME/asdf"
  export ASDF_CONFIG_FILE="$ASDF_CONFIG_DIR/asdfrc"
else
  export ASDF_CONFIG_DIR="$HOME"
  export ASDF_CONFIG_FILE="$ASDF_CONFIG_DIR/.asdfrc"
fi
export ASDF_DATA_DIR="$XDG_DATA_HOME/asdf"
export ASDF_DIR="$BREW/opt/asdf"
export ASDF_DEFAULT_TOOL_VERSIONS_FILENAME="$ASDF_CONFIG_DIR/.tool-versions"
# asdf }}}


# compilers {{{
export  CC='clang'
export CXX='clang++'
# }}}


# imagemagick {{{
export XML_CATALOG_FILES="$BREW/etc/xml/catalog"
# }}}


# ipfs {{{
#IPFS_LOGGING - sets the level of verbosity of the logging.
    #One of: debug, info, warn, error, dpanic, panic, fatal
export IPFS_LOGGING="warn"
#IPFS_LOGGING_FMT - sets formatting of the log output.
    #One of: color, nocolor
export IPFS_LOGGING_FMT="color"
# }}}


# apache stuff {{{
export APACHE_SOLR_HOME="$HOME/src/solr-8.7.0"
export NUTCH_HOME="$HOME/src/apache-nutch-2.4"
export HBASE_HOME="$HOME/src/hbase-0.98.8-hadoop2"
export HBASE_CONF_DIR="${HBASE_HOME}/conf"
export SONAR_HOME="$BREW/opt/sonar-scanner/libexec"
export SONAR="$BREW/opt/sonar-scanner/libexec/bin"
# }}}

# wxWidgets (for Audacity) {{{
if [[ "${OSX:-0}" == "${TRUE:-1}" ]]; then
  export WX_CONFIG="$BREW/x86_64/bin/wx-config"
fi
# }}}
