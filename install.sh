#!/usr/bin/env bash
#
# Dotfiles installer - sets up symlinks from home directory to dotfiles repo
#
# Usage:
#   git clone https://github.com/zfogg/dotfiles.git ~/src/github.com/zfogg/dotfiles
#   cd ~/src/github.com/zfogg/dotfiles
#   ./install.sh

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Directories
DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HOME_DIR="$HOME"

# Files/directories to skip when symlinking from root
SKIP_FILES=(
    ".git"
    ".gitignore"
    ".gitmodules"
    ".gitattributes"
    "README.md"
    "Brewfile"
    "install.sh"
    ".launchd.conf"
    ".inputrc.macos"
    "vim_aware.itermkeymap"
    "Documents"
    ".atom"
    "asdf"
    ".config"
    "bin"
)

# Platform detection
PLATFORM="unknown"
case "$(uname -s)" in
    Linux*)     PLATFORM="linux";;
    Darwin*)    PLATFORM="macos";;
    *)          PLATFORM="unknown";;
esac

log_info() {
    echo -e "${BLUE}==>${NC} $1"
}

log_success() {
    echo -e "${GREEN}✓${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

log_error() {
    echo -e "${RED}✗${NC} $1"
}

should_skip() {
    local file="$1"
    for skip in "${SKIP_FILES[@]}"; do
        if [[ "$file" == "$skip" ]]; then
            return 0
        fi
    done
    return 1
}

# Create symlink with backup
create_symlink() {
    local source="$1"
    local target="$2"
    local target_dir="$(dirname "$target")"

    # Create parent directory if needed
    if [[ ! -d "$target_dir" ]]; then
        mkdir -p "$target_dir"
    fi

    # If target exists and is already the correct symlink, skip
    if [[ -L "$target" ]] && [[ "$(readlink "$target")" == "$source" ]]; then
        log_success "$target (already linked)"
        return 0
    fi

    # If target exists (file or wrong symlink), back it up
    if [[ -e "$target" ]] || [[ -L "$target" ]]; then
        local backup="${target}.backup.$(date +%Y%m%d_%H%M%S)"
        log_warning "Backing up existing $target to $backup"
        mv "$target" "$backup"
    fi

    # Create the symlink
    ln -s "$source" "$target"
    log_success "$target -> $source"
}

main() {
    log_info "Installing dotfiles from $DOTFILES_DIR"
    log_info "Platform: $PLATFORM"
    echo

    # Step 1: Create ~/.dotfiles symlink
    log_info "Step 1: Creating ~/.dotfiles symlink"
    create_symlink "$DOTFILES_DIR" "$HOME_DIR/.dotfiles"
    echo

    # Step 2: Symlink root-level dotfiles
    log_info "Step 2: Symlinking root-level dotfiles"
    for item in "$DOTFILES_DIR"/.* "$DOTFILES_DIR"/*; do
        # Skip . and ..
        [[ "$item" == "$DOTFILES_DIR/." ]] && continue
        [[ "$item" == "$DOTFILES_DIR/.." ]] && continue
        [[ ! -e "$item" ]] && continue  # Skip broken symlinks

        local basename="$(basename "$item")"

        # Skip files in SKIP_FILES
        if should_skip "$basename"; then
            continue
        fi

        # Skip platform-specific files for other platforms
        if [[ "$PLATFORM" == "linux" ]] && [[ "$basename" == *".macos"* ]]; then
            continue
        fi

        # Only symlink files, not directories (except for bin)
        if [[ -f "$item" ]] || [[ -L "$item" ]]; then
            create_symlink "$HOME_DIR/.dotfiles/$basename" "$HOME_DIR/$basename"
        elif [[ -d "$item" ]] && [[ "$basename" == "bin" ]]; then
            create_symlink "$HOME_DIR/.dotfiles/$basename" "$HOME_DIR/$basename"
        fi
    done
    echo

    # Step 3: Symlink .config subdirectories
    log_info "Step 3: Symlinking .config subdirectories"
    if [[ -d "$DOTFILES_DIR/.config" ]]; then
        for config_item in "$DOTFILES_DIR/.config"/*; do
            [[ ! -e "$config_item" ]] && continue  # Skip if doesn't exist

            local basename="$(basename "$config_item")"

            # Skip .gitignore in .config
            [[ "$basename" == ".gitignore" ]] && continue

            # Skip base16 dirs (these are submodules, not config)
            [[ "$basename" == base16-* ]] && continue

            # Symlink the directory or file
            create_symlink "$HOME_DIR/.dotfiles/.config/$basename" "$HOME_DIR/.config/$basename"
        done
    fi
    echo

    log_success "Dotfiles installation complete!"
    echo
    log_info "Next steps:"
    echo "  • Restart your shell or run: exec \$SHELL"
    echo "  • Install vim plugins: nvim +PlugInstall +qall"
    echo "  • Review any .backup.* files created in $HOME"
}

main "$@"
