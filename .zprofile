#!/usr/bin/env zsh
# vim: set fdm=marker:


# brew's zsh {{{
export BREW='/usr/local'
export HOMEBREW_GITHUB_API_TOKEN='5c8597261ad87cf1951e4e4ce9e9c3b1d1b361be'
unalias  run-help
autoload run-help
export HELPDIR="${BREW}/share/zsh/help"
# }}}


#export      TERM="xterm-256color"
export COLORTERM="$TERM"
if [ "$TERM_PROGRAM" = "Apple_Terminal" ]; then
    export TERMINAL_DOTAPP="true"
    #setopt combiningchars # Correctly display UTF-8 with combining characters.
elif [ "$TERM_PROGRAM" = "iTerm.app" ]; then
    export ITERM_DOTAPP="true"
fi

export LC_ALL="en_US.UTF-8"
export   LANG="en_US.UTF-8"

export  TRUE="true"
export FALSE="false"

export OSX="$(
    [ "`uname`" = "Darwin" ]
    [ $? -eq 0 ] \
        && echo "$TRUE" \
        || echo "$FALSE"
)"
export SHELL_NAME="$(
    [ "$OSX" = "$TRUE" ] \
        && ps -p$$ -ocommand= | tr -d '-' \
        || ps -p$$ -ocmd=
)"

export GOPATH="$HOME/src/go"
export GOROOT="$BREW/opt/go/libexec"

export WORKON_HOME="$HOME/.virtualenvs"

export GROOVY_HOME="$BREW/opt/groovy/libexec"

export POWERLINE_CONFIG_COMMAND="python $BREW/bin/powerline-config"


# path, manpath, fpath {{{
typeset -U  path
typeset -U  fpath
typeset -U  manpath
# new var
typeset -aU classpath
typeset -xT CLASSPATH classpath
typeset -aU infopath
typeset -xT INFOPATH infopath

path=(
    $HOME/bin
    $HOME/.cabal/bin
    $HOME/.cargo/bin
    $GOPATH/bin
    /Applications/Karabiner.app/Contents/Library/bin
    $BREW/bin
    $(command -p getconf PATH | tr ':' '\n')
)

fpath=(
    $BREW/share/zsh-completions
    $BREW/share/zsh/site-functions
    $BREW/share/zsh/functions
    $fpath); fpath=($^fpath(N-/))

manpath=(
    $BREW/share/man
    /usr/share/man
); manpath=($^manpath(N-/))

infopath=()

classpath=(
    $GROOVY_HOME/embeddable
    $GROOVY_HOME/embeddable/groovy-all-2.4.7.jar
    $classpath); classpath=($^classpath(N-/))

if [ "$OSX" = "$TRUE" ]; then
    brew_gnu_progs=(
        gnu-sed
        gnu-tar
        gnu-indent
    )
    path=(
        $BREW/opt/coreutils/libexec/gnubin
        $BREW/opt/${^brew_gnu_progs}/bin
        $path)
    manpath=(
        $BREW/opt/{${^brew_gnu_progs},coreutils}/share/man
        $manpath); manpath=($^manpath(N-/))
    infopath=(
        $BREW/opt/{${^brew_gnu_progs},coreutils}/share/info
        $infopath); infopath=($^infopath(N-/))
fi
# }}}


export EDITOR='nvim'
export VISUAL="$EDITOR"
export PAGER='vimpager'
export LESS="-isMR"

export ZSHRC="$HOME/.zshrc"

# depends on `coreutils`
eval "$(gdircolors $HOME/.dircolors)"
export CLICOLOR="YES"
export LSCOLORS="$LS_COLORS"
export DOTFILES="`grealpath $(dirname "$ZSHRC")`"
export   DOTVIM="$DOTFILES/.vim"
export    ZSHRC="$DOTFILES/.zshrc"


# zsh-completion-generator.plugin.zsh
export GENCOMPL_FPATH="$DOTFILES/.zsh/complete"
