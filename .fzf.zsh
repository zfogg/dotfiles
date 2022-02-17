# Setup fzf
# ---------
local fzf_dir=${XDG_DATA_HOME}/nvim/site/pack/packer/start/fzf
path=($fzf_dir/bin $path)

# Auto-completion
# ---------------
[[ $- == *i* ]] && source $fzf_dir/shell/completion.zsh 2> /dev/null

# Key bindings
# ------------
source $fzf_dir/shell/key-bindings.zsh
