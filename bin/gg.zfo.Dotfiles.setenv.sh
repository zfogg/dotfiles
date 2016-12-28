#!/bin/sh


launchctl setenv OSX 1
launchctl setenv XDG_CONFIG_HOME $HOME/.config
 

launchctl setenv PKG_CONFIG_PATH /usr/local/lib/pkgconfig:/usr/lib/pkgconfig
launchctl setenv LC_ALL en_US.UTF-8
launchctl setenv LANG en_US.UTF-8

launchctl setenv TRUE 1
launchctl setenv FALSE 0

launchctl setenv TERM xterm-256color
launchctl setenv COLORTERM xterm-256color
launchctl setenv PACKAGE_PREFIX gg.zfo

launchctl setenv EDITOR nvim
launchctl setenv VISUAL nvim
launchctl setenv PAGER vimpager

launchctl setenv BREW /usr/local
launchctl setenv HOMEBREW_GITHUB_API_TOKEN 4952d0c01c437c4d940517acc3781fab7e43f207

launchctl setenv DOTFILES_SETENV 1 # run this last
