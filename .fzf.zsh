# Setup fzf
# ---------
if [[ ! "$PATH" == */home/debian/src/github.com/zfogg/dotfiles.git/.local/share/nvim/lazy/fzf/bin* ]]; then
  PATH="${PATH:+${PATH}:}/home/debian/src/github.com/zfogg/dotfiles.git/.local/share/nvim/lazy/fzf/bin"
fi

source <(fzf --zsh)
