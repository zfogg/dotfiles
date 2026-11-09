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

### Windows

Run the PowerShell installer from PowerShell:

```powershell
cd $HOME\src\dotfiles
.\install.ps1
```

`install.ps1` installs Scoop when it is unavailable, adds the required Scoop
buckets, and installs any missing packages from the current Scoop package set,
including Neovim. It also:

- Merges an existing `~/bin` into the repository before creating a `mklink`
  from `~/bin` to `~/src/dotfiles/bin`
- Adds `~/bin` and Scoop's shims directory to the user PATH
- Preserves an existing OS PowerShell profile rather than overwriting it
- Creates the Windows PowerShell profile link only when that profile is absent

The Windows installer is safe to rerun; installed packages and existing PATH
entries are skipped.

## Structure

- **Root dotfiles**: Shell configs, tool configs, etc. are symlinked from repo root
- **`.config/`**: Application configs are symlinked directory-by-directory
- **`bin/`**: Personal scripts and utilities
- Platform-specific files (`.inputrc.macos`, etc.) are only installed on matching platforms

## After Installation

- Restart your shell: `exec $SHELL`
- Install vim plugins: `nvim +PlugInstall +qall`
- Review any `.backup.*` files in your home directory
