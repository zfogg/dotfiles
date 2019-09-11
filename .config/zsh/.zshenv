#!/usr/bin/env zsh
# vim: set fdm=marker:


zmodload zsh/zprof

# note: meta helpers {{{
command_exists() { command -v "$1" 2>/dev/null 1>&2 }
# alias_exists()   { alias      "$1" 2>/dev/null 1>&2 }
export DISABLE_UPDATE_PROMPT=true
# }}}


# $SHELL {{{
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_DATA_HOME="${HOME}/.local/share"

export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:/usr/lib/pkgconfig"

export SHELL_NAME="`basename ${SHELL}`"
export OSX="$(
  [[ "`uname`" == "Darwin" ]]
  [[ "$?" == "0" ]] \
    && echo "${TRUE:-1}" \
    || echo "${FALSE-0}")"

export DOTFILES=~/.dotfiles
export DOTVIM=~/.vim
# $SHELL }}}

#if [[ "${OSX:-0}" == "${TRUE:-1}" ]]; then
  #export DOTFILES="`dirname "$(/usr/local/bin/grealpath "$ZSHRC")"`"
#else
  #export DOTFILES="$(dirname `realpath "$ZSHRC"`)"
#fi

export ZDOTDIR="${XDG_CONFIG_HOME:-${HOME}/.config}/zsh"
export   ZSHRC="${ZDOTDIR}/.zshrc"

export BREW='/usr/local'

export   TRUE='1'
export  FALSE='0'

# editor, pager {{{
export EDITOR='nvim'
export VISUAL="${EDITOR}"
export MANPAGER="${EDITOR} -c 'set ft=man' -"
export LESS='-R'

export PAGER='nvimpager'
#export PAGER='nvim -R'
#export PAGER='less'
# }}}


# zsh startup debug (TOP of ~/.zshenv) {{{
#   https://kev.inburke.com/kevin/profiling-zsh-startup-time
if [[ ! -z "$SHELL_DEBUG" ]]; then
  local zsh_debug_log="`mktemp`"
  SHELL_DEBUG_LOG=`grealpath "$zsh_debug_log"`
  function shell-debug-log {
    $EDITOR -u NONE +'1d' +'x' "$SHELL_DEBUG_LOG"
    (echo "# vim: fdm=marker fen:"; cat "$SHELL_DEBUG_LOG") > "$SHELL_DEBUG_LOG".tmp
    mv "$SHELL_DEBUG_LOG".tmp "$SHELL_DEBUG_LOG"
    echo "#}}}" >> "$SHELL_DEBUG_LOG"
    #sed -i 's|^#{{{#}}}$||'  "$SHELL_DEBUG_LOG"
    #sed -i 's|'"$HOME"'|~/|' "$SHELL_DEBUG_LOG"
    $EDITOR "$SHELL_DEBUG_LOG"
  }
  zmodload zsh/datetime
  setopt PROMPT_SUBST
  PS4=$'#}}}\012\012\012# %x:%I {{{\012# %N:i \012# +$EPOCHREALTIME\012  '
  exec 3>&2 2>$SHELL_DEBUG_LOG
  setopt XTRACE
fi
# zsh startup debug (TOP of ~/.zshenv) }}}


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


# launchd env init {{{
#if [[ -o interactive && "${OSX:-0}" == "${TRUE:-1}" && ! -v DOTFILES_SETENV ]]; then
  #fpath+=(~/.zsh/site-functions)
  #export FPATH
  #echo 'eval "`~/bin/local.launchd`"'
  #eval "`~/bin/local.launchd`"
  #if   [ -v ITERM_DOTAPP ];    then echo "exiting.." && sleep 5 && pkill -fla iTerm.app
  #elif [ -v TERMINAL_DOTAPP ]; then echo "exiting.." && sleep 5 && pkill -fla Terminal.app
  #fi
#fi

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

export PACKAGE_PREFIX="gg.zfo"

export  DOTFILES_SETENV="1"
# launchd env init }}}


# $SHELL help {{{
#export HELPDIR="${BREW}/share/zsh/help"
unalias run-help 2>/dev/null
autoload -U run-help
# $SHELL help }}}


# groovy {{{
#export GROOVY_HOME="$BREW/opt/groovy/libexec"
#
# groovy }}}


# golang {{{
export GOPATH="${HOME}/src/go"
export GO111MODULES='on'
if [[ "${OSX:-0}" == "${TRUE:-1}" ]]; then
    export GOROOT="${BREW}/opt/go/libexec"
else
    export GOROOT="/usr/lib/go"
fi
# }}}


# mono {{{
#export MONO_GAC_PREFIX="$BREW"
#
# mono }}}


# node {{{
#export NVM_DIR="${HOME}/.nvm"
[ -d ~/.local ] \
  && export npm_config_prefix=~/.local \
  || export npm_config_prefix=~/.npm

# node }}}


# antigen {{{
[ -d "${HOME}/.zsh/complete" ] \
    && export GENCOMPL_FPATH="${HOME}/.zsh/complete"
# antigen }}}


# python {{{
    export PIP_REQUIRE_VIRTUALENV=true
    # pyenv {{{
    #export PYENV_SHELL="$SHELL_NAME"
    export PYENV_ROOT="${HOME}/.pyenv"
    export PYENV_VIRTUALENV_DISABLE_PROMPT='1'
    export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV='true'
    # }}}

    #if [[ -n $VIRTUAL_ENV && -e "${VIRTUAL_ENV}/bin/activate" ]]; then;
      #source "${VIRTUAL_ENV}/bin/activate"; fi

    # pipenv {{{
    #export PIPENV_SHELL_COMPAT="$TRUE"
    #export PIPENV_VENV_IN_PROJECT="$TRUE"
    #export PIPENV_MAX_DEPTH='4'
    # pipenv }}}
# python }}}


# iOSOpenDev {{{
#export iOSOpenDevPath=/opt/iOSOpenDev
#export iOSOpenDevDevice=
#export iOSOpenDevDevicePort=
#export THEOS=/opt/theos
#export THEOS_DEVICE_IP=10.0.0.71
#export THEOS_DEVICE_PORT=22
#iOSOpenDev }}}


# compilers {{{
  export  CC='clang'
  export CXX='clang++'
# }}}




# compilation {{{
if [[ "$OSX" == "$TRUE" ]]; then
  # new vars
  typeset -aU dyld_library_path
  typeset -xT DYLD_LIBRARY_PATH dyld_library_path ':'

  typeset -aU library_path
  typeset -xT LIBRARY_PATH      library_path      ':'
  #
  typeset -aU cppflags
  typeset -xT CPPFLAGS          cppflags          ' '

  typeset -aU cflags
  typeset -xT CFLAGS            cflags            ' '
  #
  typeset -aU ldflags
  typeset -xT LDFLAGS           ldflags           ' '
  #
  typeset -aU pkg_config_path
  typeset -xT PKG_CONFIG_PATH pkg_config_path     ':'

  function opt_dep() {
    local opt_root="${BREW}/opt"
    local dep_root="${opt_root}/${1:-\$}"
    if [ ! -d $dep_root ]; then
      >&2 echo "${0}() - err\n\tdep_root ${dep_root}"
      return 1
    fi

    if [ ${2:-$} != '$' ]; then
      local -aU bin_dirs
      local -T BIN_DIRS bin_dirs
      BIN_DIRS="${2:-bin}"
      bin_dirs=("${dep_root}/${^bin_dirs}")
      path=("$bin_dirs" $path)
    fi

    local manpath_dir="${dep_root}/${3:-share/man}"
    if [ -d $manpath_dir ]; then
      manpath=("$manpath_dir" $manpath)
    fi
    local sharedlib_dir="${dep_root}/${4:-lib}"
    if [ -d $sharedlib_dir ]; then
      dyld_library_path+=( "${sharedlib_dir}")
      ldflags+=(         "-L${sharedlib_dir}")
      #ldflags+=("-Wl,-rpath,${sharedlib_dir}")
    fi
    local headers_dir="${dep_root}/${5:-include}"
    if [ -d $headers_dir ]; then
      cppflags+=("-I${headers_dir}")
    fi
    local pkgconfig_dir="${sharedlib_dir}/${6:-pkgconfig}"
    if [ -d $pkgconfig_dir ]; then
      pkg_config_path=("$pkgconfig_dir" $pkg_config_path)
    fi
  }
  #opt_dep libressl       '$'
  #opt_dep openssl        bin
  #opt_dep readline
  #opt_dep libgit2        '$'
  #opt_dep postgresql-9.5 bin
  #opt_dep gpg-agent      bin

  #opt_dep opencv@2       bin

  # NOTE: these break curl, homebrew, etc
  #opt_dep llvm       bin:libexec
  #opt_dep curl       bin:libexec

  export OPENSSL_ROOT_DIR="$BREW"/opt/openssl
  export OPENSSL_INCLUDE_DIR="$OPENSSL_ROOT_DIR"/include

  cflags="$cppflags"

  library_path=(
    "$BREW"/lib
    $library_path)
fi
