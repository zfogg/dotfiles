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


bundles = [
        (bundle "robbyrussell/oh-my-zsh")
            { sourcingStrategy  = antigenSourcingStrategy
            , sourcingLocations = [
                --  "plugins/battery"
                --, "plugins/branch"
                  "plugins/brew"
                , "plugins/compleat"
                --, "plugins/extract"
                --, "plugins/emoji"
                , "plugins/git"
                --, "plugins/gitfast"
                --, "plugins/gitignore"
                --, "plugins/git-extras"
                , "plugins/gnu-utils"
                --, "plugins/golang"
                --, "plugins/node"
                --, "plugins/osx"
                --, "plugins/ruby"
                --, "plugins/ssh-agent"
                --, "plugins/sudo"
                --, "plugins/tmux"
                , "plugins/vi-mode"
                --, "plugins/xcode"
            ] }

        , (bundle "zsh-users/zsh-syntax-highlighting")

        , (bundle "zsh-users/zsh-history-substring-search")

        , (bundle "zsh-users/zsh-autosuggestions")

        , (bundle "RobSis/zsh-completion-generator")

        , (bundle "nojhan/liquidprompt")

        --, (bundle "supercrabtree/k")

        -- FIXME: update to a newer fork
        --, (bundle "willghatch/zsh-hooks")
        , (bundle "RobertAudi/zsh-hooks")

        , (bundle "sharat87/zsh-vim-mode")

        --, (bundle "djui/alias-tips")

        --, (bundle "Tarrasch/zsh-autoenv")

        , (bundle "lukechilds/zsh-nvm")
    ]


config = defaultConfig { plugins = bundles }


main :: IO ()
main = antigen config

