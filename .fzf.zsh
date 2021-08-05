# Setup fzf
# ---------

#if [[ ! "$PATH" == */usr/local/opt/fzf/bin* ]]; then
#export PATH="${PATH:+${PATH}:}/usr/local/opt/fzf/bin"
#fi

# Set root for OS
# ---------
#set -x
local _z_fzf_root=/dev/null
if [[ ${LINUX:-0} == ${TRUE:-1} ]]; then
  _z_fzf_root=/usr/share/fzf
elif [[ $OSX == $TRUE ]]; then
  _z_fzf_root=/usr/local/opt/fzf/shell
fi

# Auto-completion
# ---------------
function() {
  local completion="${_z_fzf_root}/completion.zsh"
  [[ $- == *i* && -f $completion ]] && source "$completion" 2>/dev/null
}

# Key bindings
# ------------
set +x
function() {
  local completion="${_z_fzf_root}/key-bindings.zsh"
  [[ -f $keybindings ]] && source "$keybindings"
}
