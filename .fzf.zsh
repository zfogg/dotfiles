# Setup fzf
# ---------
function setup_fzf_for_zsh() {
    local xdg_data_home="${XDG_DATA_HOME:-${HOME:-}/.local/share}"
    local fzf_bin_dir="$xdg_data_home/nvim/lazy/fzf/bin"
    if [[ ! "$PATH" == *$fzf_bin_dir* ]]; then
        PATH="${PATH:+${PATH}:}$fzf_bin_dir"
    fi

    source <(fzf --zsh)
}

setup_fzf_for_zsh
