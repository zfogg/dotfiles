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
        { fpathLocations   = []
        , sourcingStrategy = filePathsSourcingStrategy [
            --, "lib/bzr.zsh"
                "lib/history.zsh"
            --, "lib/clipboard.zsh"
                , "lib/compfix.zsh"
                , "lib/completion.zsh"
            --, "lib/correction.zsh"
            --, "lib/diagnostics.zsh"
                , "lib/directories.zsh"
                , "lib/functions.zsh"
                , "lib/git.zsh"
                , "lib/grep.zsh"
                , "lib/history.zsh"
                , "lib/key-bindings.zsh"
            --, "lib/misc.zsh"
            --, "lib/nvm.zsh"
            --, "lib/prompt_info_functions.zsh"
            --, "lib/spectrum.zsh"
                , "lib/termsupport.zsh"
            --, "lib/theme-and-appearance.zsh"
            ]
        }

    ,  bundle "zsh-users/zsh-syntax-highlighting"
    , (bundle "zsh-users/zsh-history-substring-search")
        { sourcingStrategy = filePathsSourcingStrategy [
                "zsh-history-substring-search.zsh"
        ] }
    , (bundle "b4b4r07/zsh-vimode-visual")
        { sourcingStrategy = filePathsSourcingStrategy [
                "zsh-vimode-visual.zsh"
        ] }

    , bundle "nojhan/liquidprompt"

    , bundle "supercrabtree/k"

    , bundle "willghatch/zsh-hooks"
    , bundle "sharat87/zsh-vim-mode"
    ]


config = defaultConfig { plugins = bundles }


main :: IO ()
main = antigen config

