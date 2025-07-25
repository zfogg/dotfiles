# Setup fzf
# ---------
if [[ ! "$PATH" == $HOME/.local/share/nvim/site/pack/packer/start/fzf/bin* ]]; then
  PATH="${PATH:+${PATH}:}$HOME/.local/share/nvim/site/pack/packer/start/fzf/bin"
fi

source <(fzf --zsh)
