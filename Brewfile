# vi: ft=ruby fdm=marker
# INFO: ~/.dotfiles/Brewfile

cask_args appdir: "/Applications"


# tap {{{
tap "homebrew/bundle"
tap "homebrew/cask"
tap "homebrew/cask-fonts"
tap "homebrew/core"
#tap "homebrew/livecheck"
tap "homebrew/services"
tap "romkatv/powerlevel10k"
tap "homebrew-ffmpeg/ffmpeg"
tap "amiaopensource/amiaos"
# tap }}}


# mas {{{
brew "mas"

mas "Xcode",             id: 497799835
mas "Cleaner for Xcode", id: 1296084683
mas "Developer",         id: 640199958

mas "1Password 7",       id: 1333542190
mas "WhatsApp",          id: 1147396723

mas "NextDNS",           id: 1464122853
mas "Discovery",         id: 1381004916

mas "Messenger",         id: 1480068668
mas "Twitter",           id: 1482454543
mas "Vimari",            id: 1480933944
mas "slack",             id: 803453959
# mas }}}


# cask {{{
cask "iterm2"
cask "1password-cli"
cask "authy"
cask "dropbox"
cask "alfred"

cask "key-codes"

cask "google-chrome"
cask "firefox"

cask "iina"
cask "vlc"
cask "spotify"
cask "blackhole"
cask "obs"

cask "spectacle"
cask "appcleaner"

cask "discord"
cask "notion"
cask "github"

cask "vimr"

cask "disk-inventory-x"

cask "gpg-suite"
brew "pinentry-mac"

cask "font-fira-code"
cask "font-noto-sans"
cask "font-meslo-lg"
cask "font-meslolg-nerd-font"
cask "font-recursive"

cask "deluge"

cask "adoptopenjdk"
cask "adoptopenjdk"

cask "visual-studio-code"

cask "xquartz"
# cask }}}


# brew {{{
brew "cmake"
brew "openssl"
brew "coreutils"
brew "gnu-sed"
brew "gnu-tar"
brew "findutils"

brew "zsh"
brew "bash"
brew "powerlevel10k"
brew "tmux"
brew "reattach-to-user-namespace"

brew "cocoapods"
brew "p7zip"

brew "trash"

brew "vim"
brew "neovim", args: ["HEAD"]

brew "git"
brew "git-lfs"
brew "ghq"

brew "cabal-install"
brew "haskell-stack"

brew "python"
brew "python@3.8"
brew "ipython"

brew "go"

brew "ruby"

brew "pyenv"
brew "pyenv-virtualenv"
brew "nvm"

brew "rbenv"
brew "rbenv-default-gems"
brew "rbenv-vars"
brew "rbenv-bundler"

brew "rustup-init"

brew "chromaprint", args: ["ignore-dependencies"]
brew "decklinksdk"
ALL_FFMPEG_OPTIONS = `brew options homebrew-ffmpeg/ffmpeg/ffmpeg | grep -vE '\s' | grep -- '--with-'`.gsub("--", "").split("\n")
brew "homebrew-ffmpeg/ffmpeg/ffmpeg", args: ALL_FFMPEG_OPTIONS
brew "youtube-dl"

brew "ripgrep"
brew "fd"

brew "jq"
brew "lsd"
brew "fzf"
brew "tree"

brew "translate-shell"
brew "mplayer"
brew "mpv", args: ["ignore-dependencies"]
brew "mpg123"

brew "wget"
brew "curl"
brew "ldns"
brew "telnet"
brew "htop"

brew "grc"
brew "bat"
brew "pygments"

brew "docker"
brew "docker-compose"

brew "openjdk"
# brew }}}

