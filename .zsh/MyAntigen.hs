{-# LANGUAGE OverloadedStrings #-}


module Main where

import Antigen (
    -- Rudimentary imports
    AntigenConfig (..)
    , defaultConfig
    , bundle
    , antigen
    -- If you want to source a bit trickier plugins
    , ZshPlugin (..)
    , antigenSourcingStrategy
    , filePathsSourcingStrategy
    )


ohMyZshPlugins = [ "plugins/golang"
                 , "plugins/git"
                 , "plugins/httpie"
                 , "plugins/node"
                 , "plugins/extract"
                 , "plugins/safe-paste"
                 , "plugins/vi-mode"
                 , "plugins/tmux"
                 , "plugins/z"
                 , "plugins/brew"
                 , "plugins/brew-cask"
                 , "plugins/osx"
                 , "plugins/colorize"
                 ]


bundles =
    [ (bundle "robbyrussell/oh-my-zsh")
        { fpathLocations   = []
        , sourcingStrategy = filePathsSourcingStrategy [ "lib/history.zsh"
                                                       , "lib/key-bindings.zsh"
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


config = defaultConfig { plugins = bundles }


main :: IO ()
main = antigen config

