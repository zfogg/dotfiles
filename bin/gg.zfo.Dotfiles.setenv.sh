#!/bin/sh


launchctl setenv                                                       \
    OSX 1                                                              \
    CORES 4                                                            \
\
    XDG_CONFIG_HOME $HOME/.config                                      \
    PKG_CONFIG_PATH /usr/local/lib/pkgconfig:/usr/lib/pkgconfig        \
\
    LC_ALL en_US.UTF-8                                                 \
    LANG   en_US.UTF-8                                                 \
\
    TRUE  1                                                            \
    FALSE 0                                                            \
\
    TERM           xterm-256color                                      \
    COLORTERM      xterm-256color                                      \
    PACKAGE_PREFIX gg.zfo                                              \
\
    EDITOR nvim                                                        \
    VISUAL nvim                                                        \
    PAGER  vimpager                                                    \
\
    BREW                      /usr/local                               \
    HOMEBREW_GITHUB_API_TOKEN 941d5149b50fa9453a8d5094818184483057165c \
\
    DOTFILES_SETENV 1                                                  # run this last

#launchctl setenv ZDOTDIR $HOME/.zsh
