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
      (bundle "zsh-users/zsh-syntax-highlighting")
        { fpathLocations = [] }

    , (bundle "zsh-users/zsh-history-substring-search")
        { fpathLocations = []
        , sourcingStrategy = filePathsSourcingStrategy
            [ "zsh-history-substring-search.zsh" ] }

    , (bundle "zsh-users/zsh-autosuggestions")
       { fpathLocations = [] }

        , (bundle "zsh-users/zsh-autosuggestions")

        , (bundle "RobSis/zsh-completion-generator")

        , (bundle "nojhan/liquidprompt")

    , (bundle "djui/alias-tips")
        { fpathLocations = [] }

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

