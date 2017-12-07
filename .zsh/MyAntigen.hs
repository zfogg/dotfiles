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


bundles =
    [ (bundle "robbyrussell/oh-my-zsh")
        { fpathLocations    = [ ]
        , sourcingStrategy  = antigenSourcingStrategy
        , sourcingLocations =
            [
            --  "plugins/battery"
            --, "plugins/branch"
              "plugins/brew"
            , "plugins/compleat"
            , "plugins/extract"
            --, "plugins/emoji"
            , "plugins/git"
            --, "plugins/gitfast"
            --, "plugins/gitignore"
            --, "plugins/git-extras"
            , "plugins/gnu-utils"
            --, "plugins/golang"
            , "plugins/node"
            , "plugins/osx"
            --, "plugins/ruby"
            , "plugins/ssh-agent"
            --, "plugins/sudo"
            , "plugins/tmux"
            , "plugins/vi-mode"
            , "plugins/xcode"
            ] }

    , (bundle "zsh-users/zsh-syntax-highlighting")
        { fpathLocations = [] }
    , (bundle "zsh-users/zsh-history-substring-search")
        { fpathLocations = []
        , sourcingStrategy = filePathsSourcingStrategy
            [ "zsh-history-substring-search.zsh" ] }

    , (bundle "zsh-users/zsh-autosuggestions")
        { fpathLocations = [] }

    , (bundle "nojhan/liquidprompt")
        { fpathLocations = [] }

    --, (bundle "supercrabtree/k")
        --{ fpathLocations = [] }

    , (bundle "willghatch/zsh-hooks")
        { fpathLocations = [] }
    , (bundle "sharat87/zsh-vim-mode")
        { fpathLocations = [] }

    --, (bundle "djui/alias-tips")
        --{ fpathLocations = [] }

    --, (bundle "Tarrasch/zsh-autoenv")
        --{ fpathLocations = [] }
    ]


config = defaultConfig { plugins = bundles }


main :: IO ()
main = antigen config

