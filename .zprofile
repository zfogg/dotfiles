#!/usr/bin/env zsh
# vim: set fdm=marker:


# brew's zsh {{{
export ZDOTDIR="$HOME"
export   ZSHRC="$ZDOTDIR/.zshrc"
# }}}


# shell help {{{
export HELPDIR="${BREW}/share/zsh/help"
unalias run-help 2>/dev/null
autoload -U run-help
# }}}


#export TERM="xterm-256color"
export COLORTERM="$TERM"
if [[ "$TERM_PROGRAM" == "Apple_Terminal" ]]; then
    export TERMINAL_DOTAPP="true"
    # Correctly display UTF-8 with combining characters:
    setopt combiningchars
elif [[ "$TERM_PROGRAM" == "iTerm.app" ]]; then
    export ITERM_DOTAPP="true"
fi

export OSX="$(
    [[ "`uname`" == "Darwin" ]]
    [[ "$?" == "0" ]] \
        && echo "$TRUE" \
        || echo "$FALSE")"
export SHELL_NAME="$(
    [[ "$OSX" == "$TRUE" ]] \
        && ps -p$$ -ocommand= | tr -d '-' \
        || ps -p$$ -ocmd=)"

export GOPATH=~/src/go
export GOROOT="$BREW/opt/go/libexec"

export GROOVY_HOME="$BREW/opt/groovy/libexec"


# path, manpath, fpath {{{
typeset -U  path
typeset -U  fpath
typeset -U  manpath

# new vars
typeset -aU classpath
typeset -xT CLASSPATH classpath
typeset -aU infopath
typeset -xT INFOPATH infopath

path=(
    ~/bin
    $PYENV_ROOT/shims
    ~/{.cabal,.cargo}/bin
    $GOPATH/bin
    $GOROOT/bin
    $BREW/bin
    $(/usr/bin/getconf PATH | /usr/bin/tr ':' '\n' | /usr/bin/tail -r)
)

fpath=(
    ~/.zsh/site-functions
    ~/.zsh/.oh-my-zsh/plugins/*/_*(.:h)
    ~/.zsh/complete
    $BREW/share/zsh-completions
    $BREW/share/zsh/{site-functions,functions})

manpath=(
    $BREW/share/man
    /usr/share/man)

infopath=()

classpath=(
    $GROOVY_HOME/embeddable
    $GROOVY_HOME/embeddable/groovy-all-2.4.7.jar
    $classpath)

if [[ "$OSX" == "$TRUE" ]]; then
    local brew_gnu_progs=(
        coreutils
        findutils
        gnu-sed
        gnu-tar
        gnu-which)
    path=(
        $BREW/opt/${^brew_gnu_progs}/libexec/gnubin
        $BREW/opt/ccache/libexec
        $path)
    manpath=(
        $BREW/opt/${^brew_gnu_progs}/libexec/gnuman
        $manpath)
    infopath=(
        $BREW/opt/${^brew_gnu_progs}/share/info
        $infopath)
fi
# }}}


# compilation {{{
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

export OPENSSL_ROOT_DIR="$BREW"'/opt/openssl'
export OPENSSL_INCLUDE_DIR="$OPENSSL_ROOT_DIR"'/include'

cflags="$cppflags"

library_path=(
    $BREW/lib
    $library_path)
# }}}
