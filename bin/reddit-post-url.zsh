#!/usr/bin/env zsh


readonly url="${1:-"$(pbpaste)"}"

http "$url"'.json' \
    | jq --raw-output '.[0].data.children[0].data.url'
