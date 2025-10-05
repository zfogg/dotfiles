#!/usr/bin/env zsh
# vim: set fdm=marker:


# ls colors {{{
autoload -Uz colors && colors

if [[ -f ~/.LS_COLORS ]]; then
  source ~/.LS_COLORS
  export ZLS_COLORS=$LS_COLORS
fi

if [[ $OSX == $TRUE ]]; then
  export CLICOLOR=true
  #export CLICOLOR_FORCE=true
  #export LSCOLORS=gxbxhxdxfxhxhxhxhxcxcx
  export LSCOLORS=GxFxCxDxBxegedabagaced
fi

# INFO: https://www.topbug.net/blog/2016/09/27/make-gnu-less-more-powerful/
export COLUMNS
export LESS_TERMCAP_mb=$'\E[1;31m'     # begin bold
export LESS_TERMCAP_md=$'\E[1;36m'     # begin blink
export LESS_TERMCAP_me=$'\E[0m'        # reset bold/blink
export LESS_TERMCAP_so=$'\E[01;44;33m' # begin reverse video
export LESS_TERMCAP_se=$'\E[0m'        # reset reverse video
export LESS_TERMCAP_us=$'\E[1;32m'     # begin underline
export LESS_TERMCAP_ue=$'\E[0m'        # reset underline
# }}}


# zsh-syntax-highlighting {{{
if (($+ZSH_HIGHLIGHT_HIGHLIGHTERS)); then
    typeset -A ZSH_HIGHLIGHT_STYLES
    # patterns
    ZSH_HIGHLIGHT_PATTERNS+=('rm -*' fg=grey,bold,underline,bg=red)
    ZSH_HIGHLIGHT_PATTERNS+=('sudo*' fg=white,bold,bg=red)
    # styles
    ZSH_HIGHLIGHT_STYLES[default]=none
    ZSH_HIGHLIGHT_STYLES[unknown-token]=fg=248
    ZSH_HIGHLIGHT_STYLES[reserved-word]=fg=009,standout
    ZSH_HIGHLIGHT_STYLES[alias]=fg=cyan,bold
    ZSH_HIGHLIGHT_STYLES[builtin]=fg=145,bold,underline
    ZSH_HIGHLIGHT_STYLES[function]=fg=cyan,bold
    ZSH_HIGHLIGHT_STYLES[command]=fg=white,bold
    ZSH_HIGHLIGHT_STYLES[precommand]=fg=white,underline
    ZSH_HIGHLIGHT_STYLES[commandseparator]=none
    ZSH_HIGHLIGHT_STYLES[hashed-command]=fg=009,standout
    ZSH_HIGHLIGHT_STYLES[path]=fg=204,underline
    ZSH_HIGHLIGHT_STYLES[globbing]=fg=063
    ZSH_HIGHLIGHT_STYLES[history-expansion]=fg=underline
    ZSH_HIGHLIGHT_STYLES[single-hyphen-option]=fg=137,bold
    ZSH_HIGHLIGHT_STYLES[double-hyphen-option]=fg=244,italic
    ZSH_HIGHLIGHT_STYLES[back-quoted-argument]=fg=244,underline
    ZSH_HIGHLIGHT_STYLES[single-quoted-argument]=fg=137,bold
    ZSH_HIGHLIGHT_STYLES[double-quoted-argument]=fg=244
    ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]=fg=148,bold
    ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]=fg=148,bold
    ZSH_HIGHLIGHT_STYLES[assign]=fg=246
fi
# }}}


# base16-shell / Base16 Shell {{{
export BASE16_SHELL=$HOME/.config/base16-shell
if [[ ! $TERM =~ kitty ]]; then
  if [ -n "$PS1" ] && [ -s "$BASE16_SHELL/profile_helper.sh" ]; then
    source "$BASE16_SHELL/profile_helper.sh"
    #command_exists base16_default-dark && base16_default-dark
    #_base16 "$HOME/.config/base16-shell/scripts/base16-default-dark.sh" default-dark
    #_base16 "$HOME/.config/base16-shell/scripts/base16-bright.sh" bright
    #function exit() {
    #    [ -f ~/.base16_theme ] && source ~/.base16_theme
    #    builtin exit
    #}
  fi
fi
# }}}

