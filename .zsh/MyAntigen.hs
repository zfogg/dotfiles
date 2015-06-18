{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}


module MyAntigen where

import Antigen (
    -- Rudimentary imports
    AntigenConfiguration (..)
    , bundle
    , antigen
    -- If you want to source a bit trickier plugins
    , ZshPlugin (..)
    , antigenSourcingStrategy
    , filePathsSourcingStrategy
    )

import Shelly (shelly)


bundles =
    [ (bundle "robbyrussell/oh-my-zsh")
        { sourcingLocations = [ "plugins/aws"
                              , "plugins/golang"
                              , "plugins/httpie"
                              , "plugins/tmux"
                              , "plugins/git"
                              , "plugins/git-extras"
                              --, "plugins/docker"
                              , "plugins/docker-compose"
                              , "plugins/node"
                              , "plugins/npm"
                              , "plugins/pip"
                              , "plugins/python"
                              , "plugins/colored-man"
                              , "plugins/extract"
                              , "plugins/safe-paste"
                              , "plugins/vi-mode"
                              , "plugins/z"
                              , "plugins/brew"
                              , "plugins/brew-cask"
                              , "plugins/osx"
                              , "plugins/colorize"] }

    , bundle "nojhan/liquidprompt"
    , bundle "hchbaw/opp.zsh"
    , bundle "sharat87/zsh-vim-mode"
    , bundle "zsh-users/zsh-syntax-highlighting"
    , bundle "zsh-users/zsh-history-substring-search" ]

config = AntigenConfiguration bundles

main :: IO ()
main = shelly $ antigen config
