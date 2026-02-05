#!/usr/bin/env zsh
# vim: set fdm=marker:


bindkey -M viins 'jj'              vi-cmd-mode
bindkey -M viins 'kk' last-cmd-and-vi-cmd-mode

bindkey -M viins 'jj'              vi-cmd-mode
bindkey -M viins 'kk' last-cmd-and-vi-cmd-mode

bindkey -rM vicmd ':'
bindkey  -M vicmd ':' execute-named-cmd # ; line vim

# useful motion shortcuts
bindkey -M viins 'hH'    beginning-of-line
bindkey -M viins 'lL'          end-of-line
bindkey -M vicmd 'hH' vi-beginning-of-line
bindkey -M vicmd 'lL'       vi-end-of-line

bindkey -r        '^H'
bindkey -rM viins '^H'
bindkey -rM vicmd '^H'
bindkey -r        '^J'
bindkey -rM viins '^J'
bindkey -rM vicmd '^J'
bindkey -r        '^L'
bindkey -rM viins '^L'
bindkey -rM vicmd '^L'

bindkey          '^[[Z' reverse-menu-complete

# <M-Direction>
bindkey          '^[[1;3D'    backward-word
bindkey -M vicmd '^[[1;3D' vi-backward-word
bindkey -M viins '^[[1;3D'    backward-word
bindkey          '^[[1;3C'     forward-word
bindkey -M vicmd '^[[1;3C'  vi-forward-word
bindkey -M viins '^[[1;3C'     forward-word

# <S-Backspace>
bindkey          '^[[3~'       delete-char
bindkey -M viins '^[[3~'    vi-delete-char
bindkey -M vicmd '^[[3~'    vi-delete-char
# <C-Backspace>
bindkey          '^W'    backward-kill-word
bindkey -M viins '^W'    backward-kill-word
bindkey -M vicmd '^W' vi-backward-kill-word
# <C-S-Backspace>
bindkey          '^[[2J'            kill-word
#bindkey -M vicmd '^[[2J' vi-forward-kill-word
# <M-Backspace>
bindkey            '^U' backward-kill-line
bindkey -M vicmd   '^U' backward-kill-line
bindkey -M viins   '^U' vi-kill-line
bindkey            '^K'    kill-line
bindkey -M vicmd   '^K' vi-kill-eol
bindkey -M viins   '^K' vi-change-eol

# <Up>, <Down>
bindkey          '^[[A' history-substring-search-up
bindkey          '^[[B' history-substring-search-down
bindkey          '^[[a' history-beginning-search-backward
bindkey          '^[[b' history-beginning-search-forward
# FIXME

# <Home>, <End>
bindkey '^[[H'  beginning-of-line
bindkey '^[[F'        end-of-line
bindkey '^[[5~'   up-line-or-history
bindkey '^[[6~' down-line-or-history

# <Option-Left>, <Option-Right>
bindkey -M viins '^[b'    backward-word
bindkey -M viins '^[f'     forward-word
bindkey -M vicmd '^[b' vi-backward-word
bindkey -M vicmd '^[f'  vi-forward-word

# zsh vi-mode fixes
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic
bindkey -M viins ' '   magic-space

function _last-cmd-and-vi-cmd-mode {
    zle vi-cmd-mode
    zle history-substring-search-up
}
zle -N last-cmd-and-vi-cmd-mode _last-cmd-and-vi-cmd-mode


# zsh-users/zsh-autosuggestions {{{
[ -f ~/.zsh/zsh-autosuggestions.config.zsh ] && \
  .  ~/.zsh/zsh-autosuggestions.config.zsh

bindkey -M viins '^ '   autosuggest-accept
bindkey -M vicmd '^ '   autosuggest-accept
bindkey -M viins 'l;'   autosuggest-accept
bindkey -M viins '^[[c' autosuggest-execute

# edit-command-line
function _autosuggest-accept-and-edit-command-line {
  zle autosuggest-accept
  zle edit-command-line
}
zle -N autosuggest-accept-and-edit-command-line \
      _autosuggest-accept-and-edit-command-line

autoload -Uz edit-command-line
zle -N       edit-command-line

bindkey '^[[v' edit-command-line
bindkey '^[[v' autosuggest-accept-and-edit-command-line
# zsh-users/zsh-autosuggestions }}}

# Ctrl+1 (via Alt+1) -> insert entire previous command
function _insert-prev-command {
  LBUFFER+='!!'
  zle expand-or-complete
}
zle -N _insert-prev-command
bindkey '^[1' _insert-prev-command

# Ctrl+2 (via Alt+2) -> insert first argument of previous command (position 2)
function _insert-first-arg {
  LBUFFER+='!!:1'
  zle expand-or-complete
}
zle -N _insert-first-arg
bindkey '^[2' _insert-first-arg

# Ctrl+3 (via Alt+3) -> insert second argument of previous command (position 3)
function _insert-second-arg {
  LBUFFER+='!!:2'
  zle expand-or-complete
}
zle -N _insert-second-arg
bindkey '^[3' _insert-second-arg

# Ctrl+4 (via Alt+4) -> insert last argument of previous command
function _insert-last-arg {
  LBUFFER+='!!$'
  zle expand-or-complete
}
zle -N _insert-last-arg
bindkey '^[4' _insert-last-arg

# Ctrl+8 (via Alt+8) -> insert all arguments except command (since * is on 8)
function _insert-all-args {
  LBUFFER+='!!*'
  zle expand-or-complete
}
zle -N _insert-all-args
bindkey '^[8' _insert-all-args
