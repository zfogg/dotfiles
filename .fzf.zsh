# Setup fzf
# ---------
if [[ ! "$PATH" == */home/zfogg/.local/share/nvim/lazy/fzf/bin* ]]; then
  PATH="${PATH:+${PATH}:}/home/zfogg/.local/share/nvim/lazy/fzf/bin"
fi

source <(fzf --zsh)
