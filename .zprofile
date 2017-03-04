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


# colors {{{
#   from: http://mediadoneright.com/content/ultimate-git-ps1-bash-prompt

# Reset
export color_off="\[\033[0m\]"       # Text Reset

# Regular Colors
export color_black="\[\033[0;30m\]"        # Black
export color_red="\[\033[0;31m\]"          # Red
export color_green="\[\033[0;32m\]"        # Green
export color_yellow="\[\033[0;33m\]"       # Yellow
export color_blue="\[\033[0;34m\]"         # Blue
export color_purple="\[\033[0;35m\]"       # Purple
export color_cyan="\[\033[0;36m\]"         # Cyan
export color_white="\[\033[0;37m\]"        # White

# Bold
export color_b_black="\[\033[1;30m\]"       # Black
export color_b_red="\[\033[1;31m\]"         # Red
export color_b_green="\[\033[1;32m\]"       # Green
export color_b_yellow="\[\033[1;33m\]"      # Yellow
export color_b_blue="\[\033[1;34m\]"        # Blue
export color_b_purple="\[\033[1;35m\]"      # Purple
export color_b_cyan="\[\033[1;36m\]"        # Cyan
export color_b_white="\[\033[1;37m\]"       # White

# Underline
export color_u_black="\[\033[4;30m\]"       # Black
export color_u_red="\[\033[4;31m\]"         # Red
export color_u_green="\[\033[4;32m\]"       # Green
export color_u_yellow="\[\033[4;33m\]"      # Yellow
export color_u_blue="\[\033[4;34m\]"        # Blue
export color_u_purple="\[\033[4;35m\]"      # Purple
export color_u_cyan="\[\033[4;36m\]"        # Cyan
export color_u_white="\[\033[4;37m\]"       # White

# Background
export color_bg_black="\[\033[40m\]"       # Black
export color_bg_red="\[\033[41m\]"         # Red
export color_bg_green="\[\033[42m\]"       # Green
export color_bg_yellow="\[\033[43m\]"      # Yellow
export color_bg_blue="\[\033[44m\]"        # Blue
export color_bg_purple="\[\033[45m\]"      # Purple
export color_bg_cyan="\[\033[46m\]"        # Cyan
export color_bg_white="\[\033[47m\]"       # White

# High Intensty
export color_i_black="\[\033[0;90m\]"       # Black
export color_i_red="\[\033[0;91m\]"         # Red
export color_i_green="\[\033[0;92m\]"       # Green
export color_i_yellow="\[\033[0;93m\]"      # Yellow
export color_i_blue="\[\033[0;94m\]"        # Blue
export color_i_purple="\[\033[0;95m\]"      # Purple
export color_i_cyan="\[\033[0;96m\]"        # Cyan
export color_i_white="\[\033[0;97m\]"       # White

# Bold High Intensty
export color_bi_black="\[\033[1;90m\]"      # Black
export color_bi_red="\[\033[1;91m\]"        # Red
export color_bi_green="\[\033[1;92m\]"      # Green
export color_bi_yellow="\[\033[1;93m\]"     # Yellow
export color_bi_blue="\[\033[1;94m\]"       # Blue
export color_bi_purple="\[\033[1;95m\]"     # Purple
export color_bi_cyan="\[\033[1;96m\]"       # Cyan
export color_bi_white="\[\033[1;97m\]"      # White

# High Intensty backgrounds
export color_bg_iblack="\[\033[0;100m\]"   # Black
export color_bg_ired="\[\033[0;101m\]"     # Red
export color_bg_igreen="\[\033[0;102m\]"   # Green
export color_bg_iyellow="\[\033[0;103m\]"  # Yellow
export color_bg_iblue="\[\033[0;104m\]"    # Blue
export color_bg_ipurple="\[\033[10;95m\]"  # Purple
export color_bg_icyan="\[\033[0;106m\]"    # Cyan
export color_bg_iwhite="\[\033[0;107m\]"   # White
# }}}
