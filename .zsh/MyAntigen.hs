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
            { fpathLocations = [] }

        , (bundle "zsh-users/zsh-autosuggestions")
            { fpathLocations = [] }

        , (bundle "clarketm/zsh-completions")
            { fpathLocations = [] }

        --, (bundle "RobSis/zsh-completion-generator")
            --{ fpathLocations = [] }

        , (bundle "nojhan/liquidprompt")
            { fpathLocations = [] }

        --, (bundle "djui/alias-tips")
            --{ fpathLocations = [] }

        -- INFO: updated to a newer fork
        , (bundle "willghatch/zsh-hooks")
            { fpathLocations = [] }

        -- INFO: updated to a newer fork
        , (bundle "softmoth/zsh-vim-mode")
            { fpathLocations = [] }

        , (bundle "davidparsson/zsh-pyenv-lazy")
            { fpathLocations = [] }

        , (bundle "agkozak/zsh-z")

        --, (bundle "Tarrasch/zsh-autoenv")
            --{ fpathLocations = [] }

        , (bundle "lukechilds/zsh-nvm")
            { fpathLocations = [] }
    ]


config = defaultConfig { plugins = bundles }


main :: IO ()
main = antigen config

