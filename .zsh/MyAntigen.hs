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
          (bundle "wookayin/fast-syntax-highlighting")
            { fpathLocations = [] }

        , (bundle "zsh-users/zsh-history-substring-search")
            { fpathLocations = [] }

        , (bundle "zsh-users/zsh-autosuggestions")
            { fpathLocations = [] }

        -- INFO: aggregates zsh completions from lots of places
        {-, (bundle "clarketm/zsh-completions")-}
            {-{ sourcingStrategy = filePathsSourcingStrategy [-}
                {-] }-}
            {-{ fpathLocations = [] }-}

        {-, (bundle "RobSis/zsh-completion-generator")-}
            {-{ fpathLocations = [] }-}

        {-, (bundle "nojhan/liquidprompt")-}
            {-{ fpathLocations = [] }-}

        , (bundle "romkatv/powerlevel10k")
            { sourcingStrategy = filePathsSourcingStrategy [
                 "config/p10k-pure.zsh"
                ,"powerlevel10k.zsh-theme"
                ] }
            { fpathLocations = [] }

        --, (bundle "djui/alias-tips")
            --{ fpathLocations = [] }

        -- INFO: updated to a newer fork
        , (bundle "willghatch/zsh-hooks")
            { fpathLocations = [] }

        -- INFO: updated to a newer fork
        , (bundle "softmoth/zsh-vim-mode")
            { fpathLocations = [] }

        --, (bundle "davidparsson/zsh-pyenv-lazy")
            --{ fpathLocations = [] }

        , (bundle "agkozak/zsh-z")
            { fpathLocations = [""] }

        --, (bundle "b4b4r07/emoji-cli")
            --{ fpathLocations = [""] }

        --, (bundle "Tarrasch/zsh-autoenv")
            --{ fpathLocations = [] }

        --, (bundle "lukechilds/zsh-nvm")
            --{ fpathLocations = [] }

        --, (bundle "sobolevn/wakatime-zsh-plugin")
            --{ fpathLocations = [] }
    ]


config = defaultConfig { plugins = bundles }


main :: IO ()
main = antigen config

