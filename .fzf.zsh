# Setup fzf
# ---------
if [[ ! "$PATH" == */Users/zfogg/src/github.com/zfogg/dotfiles/.local/share/nvim/site/pack/packer/start/fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/Users/zfogg/src/github.com/zfogg/dotfiles/.local/share/nvim/site/pack/packer/start/fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/Users/zfogg/src/github.com/zfogg/dotfiles/.local/share/nvim/site/pack/packer/start/fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/Users/zfogg/src/github.com/zfogg/dotfiles/.local/share/nvim/site/pack/packer/start/fzf/shell/key-bindings.zsh"
