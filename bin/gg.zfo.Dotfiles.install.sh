#!/bin/bash


trap "exit" INT


echo "Installing @zfogg's dotfiles . . . "
echo


list_contains() {
  # http://stackoverflow.com/a/8574392
  local e
  for e in "${@:2}"; do [[ "$e" == "$1" ]] && return 0; done
  return 1
}


# Homebrew
which brew 1>/dev/null 2>&1 || ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew_installed_list=$(brew ls | tr '\t' '\r')
brew_install() {
    if ! echo "$brew_installed_list" | grep -q "^$1$" >/dev/null; then
        echo "brew_install - '$1' installing . . . "
        if brew install $1; then
	    echo "brew_install - '$1' installed."
	else
	    echo "brew_install - '$1' installation failed."
	    exit 1
        fi
    else
        echo "brew_install - '$1' - already installed."
    fi
}

# Homebrew - Cask
brew cask 1>/dev/null 2>&1 || brew install caskroom/cask/brew-cask
brew_cask_installed_list=$(brew cask ls | tr '\t' '\r')
brew_cask_install() {
    if ! echo "$brew_cask_installed_list" | grep -q "^$1$" >/dev/null; then
        echo "brew_cask_install - '$1' installing . . . "
        if brew cask install $1; then
	    echo "brew_cask_install - '$1' installed."
	else
	    echo "brew_cask_install - '$1' installation failed."
	    exit 1
        fi
    else
        echo "brew_cask_install - '$1' - already installed."
    fi
}


echo "1Password"
echo -e "\tNote to self - Use the Mac App Store. Trust me."
echo -e "\t\tDon't trust me? Curious?"
echo -e "\t\thttps://support.1password.com/invalid-code-signature-osx/?process_bundle_id=org.chromium.Chromium\n"


echo "Chromium + Flash"
echo -e "\tNote to self - Use the FreeSMUG build. Trust me."
echo -e "\t\thttp://www.freesmug.org/chromium\n"


# Utils
brew_install coreutils
brew install findutils  --with-default-names
brew install gnu-indent --with-default-names
brew install gnu-sed    --with-default-names
brew install gnutls     --with-default-names
brew install grep       --with-default-names
brew install gnu-tar    --with-default-names
brew_install gawk


brew_install cmake
brew_install colormake
brew install boost
brew install boost-python


brew_install openssl
brew link openssl --force
brew install python --with-brewed-openssl
pip install -U pip setuptools pygments neovim psutil


brew_install ruby
brew_install brew-gems
gem install git-up


which nvim >/dev/null || brew tap neovim/neovim
brew_install neovim


brew_install the_silver_searcher
brew_install tmux
brew_install ghc
brew_install cabal-install
brew_install vimpager


brew_cask_install dropbox
brew_cask_install iterm2
brew_cask_install spectacle


brew_install nvm
nvm install v4.3.0
npm install -g eslint@2.0.0-rc.1 \
    babel-eslint \
    eslint-config-standard \
    eslint-config-standard-react \
    eslint-plugin-standard \
    eslint-plugin-react \
    eslint-plugin-promise


echo
echo "Installed @zfogg's dotfiles!"

