#!/usr/bin/env zsh
# vim: set fdm=marker:


# brew's zsh {{{
export ZDOTDIR="$HOME"
export   ZSHRC="$ZDOTDIR/.zshrc"
# }}}


# shell help {{{
export HELPDIR="${BREW}/share/zsh/help"
unalias run-help 2>/dev/null
autoload run-help
# }}}


#export TERM="xterm-256color"
export COLORTERM="$TERM"
if [[ "$TERM_PROGRAM" == "Apple_Terminal" ]]; then
    export TERMINAL_DOTAPP="true"
    #setopt combiningchars # Correctly display UTF-8 with combining characters.
elif [[ "$TERM_PROGRAM" == "iTerm.app" ]]; then
    export ITERM_DOTAPP="true"
fi

export DOTFILES_PACKAGE="$PACKAGE_PREFIX"'.Dotfiles'

export OSX="$(
    [[ "`uname`" == "Darwin" ]]
    [[ "$?" == "0" ]] \
        && echo "$TRUE" \
        || echo "$FALSE"
)"
export SHELL_NAME="$(
    [[ "$OSX" == "$TRUE" ]] \
        && ps -p$$ -ocommand= | tr -d '-' \
        || ps -p$$ -ocmd=
)"

export GOPATH="$HOME/src/go"
export GOROOT="$BREW/opt/go/libexec"

export WORKON_HOME="$HOME/.virtualenvs"
export GROOVY_HOME="$BREW/opt/groovy/libexec"
export POWERLINE_CONFIG_COMMAND="python $BREW/bin/powerline-config"


# pyenv {{{
export PYENV_ROOT="$HOME/.pyenv"
export PYENV_VIRTUALENV_DISABLE_PROMPT='1'
export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV="true"
export PYTHON_CONFIGURE_OPTS=$(echo \
    "--enable-framework" \
    "--enable-ipv6" \
    "--enable-toolbox-glue" \
    "--enable-big-digits" \
    "--enable-unicode" \
    "--with-thread" \
    "CC=clang")
#export PYTHON_CONFIGURE_OPTS="--enable-framework"
#export PYTHON_CONFIGURE_OPTS="--enable-shared"

if which pyenv > /dev/null 2>&1; then
    eval "$(pyenv            init -)"
    eval "$(pyenv virtualenv-init -)"
fi
# }}}


# path, manpath, fpath {{{
typeset -U  path
typeset -U  fpath
typeset -U  manpath

# new vars
#   CLASSPATH
typeset -aU classpath
typeset -xT CLASSPATH classpath
#   INFOPATH
typeset -aU infopath
typeset -xT INFOPATH infopath

path=(
    $HOME/bin
    $PYENV_ROOT/shims
    $HOME/{.cabal,.cargo}/bin
    $GOPATH/bin
    $GOROOT/bin
    $BREW/bin
    $PYENV_ROOT/shims
    $(/usr/bin/getconf PATH | /usr/bin/tr ':' '\n' | /usr/bin/tail -r)
)

fpath=(
    $HOME/.zsh/{site-functions,functions}
    $BREW/share/zsh-completions
    $BREW/share/zsh/{site-functions,functions}
    $fpath)

manpath=(
    $BREW/share/man
    /usr/share/man
)

infopath=()

classpath=(
    $GROOVY_HOME/embeddable
    $GROOVY_HOME/embeddable/groovy-all-2.4.7.jar
    $classpath)

if [[ "$OSX" == "$TRUE" ]]; then
    brew_gnu_progs=(
        gnu-sed
        gnu-tar
        gnu-indent
    )
    path=(
        $BREW/opt/coreutils/libexec/gnubin
        $BREW/opt/ccache/libexec
        $BREW/opt/${^brew_gnu_progs}/gnubin
        $path)
    manpath=(
        $BREW/opt/coreutils/share/man
        $BREW/opt/${^brew_gnu_progs}/libexec/gnuman
        $manpath)
    infopath=(
        $BREW/opt/coreutils/share/info
        $BREW/opt/${^brew_gnu_progs}/share/info
        $infopath)
fi
# }}}


# compilation {{{
typeset -aU dyld_library_path
typeset -xT DYLD_LIBRARY_PATH dyld_library_path ':'
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


cppflags+=( '-O2' )


function opt_dep() {
    local opt_root=${BREW}/opt
    local dep_root=${opt_root}/${1:-\$}
    if [ ! -d $dep_root ]; then
        >&2 echo "$0() - err\n\tdep_root $dep_root"
        return 1
    fi

    if [ ${2:-$} != '$' ]; then
        local -aU bin_dirs
        local -T BIN_DIRS bin_dirs
        BIN_DIRS=${2:-bin}
        bin_dirs=(${dep_root}/${^bin_dirs})
        path=($bin_dirs $path)
    fi

    local manpath_dir=${dep_root}/${3:-share/man}
    if [ -d $manpath_dir ]; then
        manpath=($manpath_dir $manpath)
    fi
    local sharedlib_dir=${dep_root}/${4:-lib}
    if [ -d $sharedlib_dir ]; then
        dyld_library_path+=(  $sharedlib_dir )
        ldflags+=(          -L$sharedlib_dir )
        ldflags+=( -Wl,-rpath,$sharedlib_dir )
    fi
    local headers_dir=${dep_root}/${5:-include}
    if [ -d $headers_dir ]; then
        cppflags+=( -I$headers_dir )
    fi
    local pkgconfig_dir=${sharedlib_dir}/${6:-pkgconfig}
    if [ -d $pkgconfig_dir ]; then
        pkg_config_path=($pkgconfig_dir $pkg_config_path)
    fi
}

opt_dep libressl       '$'
opt_dep openssl        bin
opt_dep readline
opt_dep libgit2        '$'
opt_dep postgresql-9.5 bin

# NOTE: these break curl, homebrew, etc
#opt_dep llvm       bin:libexec
#opt_dep curl       bin:libexec

cflags="$cppflags"

# }}}


export EDITOR='nvim'
export VISUAL="$EDITOR"
export PAGER='vimpager'
export VIMPAGER_VIM="`which nvim`"
export VIMPAGER_RC="$HOME/.vimpagerrc"


function command_exists() { command -v "$1" 2>/dev/null 1>&2 }
function alias_exists()   { alias      "$1" 2>/dev/null 1>&2 }

if command_exists pt; then
    export GREPPRG_PRG='pt'
    export GREPPRG_ARGS='-S --home-ptignore --global-gitignore --depth=15 -f --hidden'
elif command_exists ag; then
    export GREPPRG_PRG='ag'
    export GREPPRG_ARGS='--hidden --all-text --pager ${PAGER} --follow'
elif ! alias_exists grep; then
    export GREPPRG_PRG='grep'
    export GREPPRG_ARGS='--color=auto --exclude=*.pyc --exclude-dir=.git'
fi

if [ -n "${GREPPRG_PRG+x}" ] && [ -n"${GREPPRG_ARGS+x}" ]; then
    export GREPPRG='command '"$GREPPRG_PRG"' '"$GREPPRG_ARGS"
else
    export GREPPRG="${GREPPRG:-command grep}"
fi


# depends on `coreutils`
export DOTFILES="`dirname "$(grealpath "$ZSHRC")"`"
export   DOTVIM="$DOTFILES/.vim"
# depends on `coreutils`
[[ ! -f ~/.LS_COLORS ]] && \
    gdircolors $HOME/.dircolors > ~/.LS_COLORS
source ~/.LS_COLORS
export CLICOLOR=true
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx


# zsh-completion-generator.plugin.zsh
export GENCOMPL_FPATH="$DOTFILES/.zsh/complete"

export FZF_DEFAULT_COMMAND='ag -i -g ""'


# postgresql
export     PGDATA="$BREW/var/postgres"
export     PGHOST="localhost"
export PGHOSTADDR="127.0.0.1"
export     PGPORT="5432"
