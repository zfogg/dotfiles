# Setup fzf
# ---------
if [[ ! "$PATH" == */Users/zfogg/.local/share/nvim/site/pack/packer/start/fzf/bin* ]]; then
  PATH="${PATH:+${PATH}:}/Users/zfogg/.local/share/nvim/site/pack/packer/start/fzf/bin"
fi

source <(fzf --zsh)
