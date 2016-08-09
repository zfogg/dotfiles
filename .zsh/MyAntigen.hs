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


ohMyZshPlugins = [-- "plugins/golang"
                 --, "plugins/git"
                 --, "plugins/httpie"
                 --  "plugins/node"
                   "plugins/extract"
                 , "plugins/safe-paste"
                 , "plugins/vi-mode"
                 --, "plugins/tmux"
                 --, "plugins/z"
                 --, "plugins/brew"
                 --, "plugins/brew-cask"
                 --, "plugins/osx"
                 --, "plugins/colorize"
                 ]


bundles =
    [ (bundle "robbyrussell/oh-my-zsh")
        { fpathLocations   = []
        , sourcingStrategy = filePathsSourcingStrategy [ "lib/history.zsh"
                                                       , "lib/key-bindings.zsh"
                                                       --, "lib/theme-and-appearance.zsh"
                                                       , "lib/completion.zsh"
                                                       ] }

    , (bundle "zsh-users/zsh-history-substring-search")
        { sourcingStrategy = filePathsSourcingStrategy [ "zsh-history-substring-search.zsh" ]}

    , (bundle "robbyrussell/oh-my-zsh")
        { fpathLocations    = ohMyZshPlugins
        , sourcingLocations = ohMyZshPlugins }

    , bundle "nojhan/liquidprompt"

    , (bundle "rupa/z")
        { sourcingStrategy = filePathsSourcingStrategy [ "z.sh" ] }
    , bundle "supercrabtree/k"

    --, bundle "zfogg/zsh-syntax-highlighting-filetypes"
    , bundle "zsh-users/zsh-syntax-highlighting"

    , bundle "willghatch/zsh-hooks"
    , bundle "sharat87/zsh-vim-mode"
    ]


config = defaultConfig { plugins = bundles }


main :: IO ()
main = antigen config

