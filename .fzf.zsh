# Setup fzf
# ---------
if [[ ! "$PATH" == */Users/zfogg/.local/share/nvim/lazy/fzf/bin* ]]; then
  PATH="${PATH:+${PATH}:}/Users/zfogg/.local/share/nvim/lazy/fzf/bin"
fi

source <(fzf --zsh)
