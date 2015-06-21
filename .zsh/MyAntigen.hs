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


ohMyZshPlugins = [ "plugins/golang"
                 , "plugins/git"
                 , "plugins/httpie"
                 , "plugins/docker-compose"
                 , "plugins/node"
                 , "plugins/pip"
                 , "plugins/colored-man"
                 , "plugins/extract"
                 , "plugins/safe-paste"
                 , "plugins/vi-mode"
                 , "plugins/z"
                 , "plugins/brew"
                 , "plugins/brew-cask"
                 , "plugins/osx"
                 , "plugins/colorize"
                 ]


bundles =
    [ (bundle "robbyrussell/oh-my-zsh")
        { fpathLocations   = []
        , sourcingStrategy = filePathsSourcingStrategy [ "lib/completion.zsh"
                                                       , "lib/correction.zsh"
                                                       , "lib/directories.zsh"
                                                       , "lib/history.zsh"
                                                       , "lib/key-bindings.zsh"
                                                       , "lib/termsupport.zsh"
                                                       , "lib/theme-and-appearance.zsh"
                                                       ] }

    , (bundle "robbyrussell/oh-my-zsh")
        { fpathLocations    = ohMyZshPlugins
        , sourcingLocations = ohMyZshPlugins }

    , bundle "nojhan/liquidprompt"
    , bundle "hchbaw/opp.zsh"
    , (bundle "rupa/z")
        { sourcingStrategy = filePathsSourcingStrategy [ "z.sh" ] }
    , bundle "sharat87/zsh-vim-mode"
    , bundle "zsh-users/zsh-syntax-highlighting"
    , bundle "zsh-users/zsh-history-substring-search"
    , bundle "supercrabtree/k"
    ]


config = AntigenConfiguration bundles


main :: IO ()
main = shelly $ antigen config

