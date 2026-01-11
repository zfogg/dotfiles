zfogg's dotfiles
================

My dotfiles. Feel free to browse, fork, or even push.

## Installation

```bash
# Clone the repo
git clone https://github.com/zfogg/dotfiles.git ~/src/github.com/zfogg/dotfiles

# Run the installer
cd ~/src/github.com/zfogg/dotfiles
./install.sh
```

The installer will:
- Create `~/.dotfiles` symlink pointing to the repo
- Symlink all root-level dotfiles (`.zshrc`, `.tmux.conf`, etc.) to your home directory
- Symlink `.config/*` subdirectories to `~/.config/`
- Back up any existing files with a timestamp

## Structure

- **Root dotfiles**: Shell configs, tool configs, etc. are symlinked from repo root
- **`.config/`**: Application configs are symlinked directory-by-directory
- **`bin/`**: Personal scripts and utilities
- Platform-specific files (`.inputrc.macos`, etc.) are only installed on matching platforms

## After Installation

- Restart your shell: `exec $SHELL`
- Install vim plugins: `nvim +PlugInstall +qall`
- Review any `.backup.*` files in your home directory
