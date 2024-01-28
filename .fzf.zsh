# Setup fzf
# ---------
if [[ ! "$PATH" == */Users/loomen/src/github.com/zfogg/dotfiles/.local/share/nvim/site/pack/packer/start/fzf/bin* ]]; then
  PATH="${PATH:+${PATH}:}/Users/loomen/src/github.com/zfogg/dotfiles/.local/share/nvim/site/pack/packer/start/fzf/bin"
fi

# Auto-completion
# ---------------
source "/Users/loomen/src/github.com/zfogg/dotfiles/.local/share/nvim/site/pack/packer/start/fzf/shell/completion.zsh"

# Key bindings
# ------------
source "/Users/loomen/src/github.com/zfogg/dotfiles/.local/share/nvim/site/pack/packer/start/fzf/shell/key-bindings.zsh"
